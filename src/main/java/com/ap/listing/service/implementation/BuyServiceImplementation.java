/**
 * Proprietary License Agreement
 * <p>
 * Copyright (c) 2025 Advert Pulse
 * <p>
 * This software is the confidential and proprietary property of Advert Pulse
 * and is licensed, not sold. The application owner is Advert Pulse, and the
 * developer and maintainer is Bloggios. Only authorized Bloggios administrators
 * are permitted to copy, modify, distribute, or sublicense this software under
 * the terms set forth in this agreement.
 * <p>
 * You may not:
 * 1. Copy, modify, distribute, or sublicense this software without express
 *    written permission from Advert Pulse or Bloggios.
 * 2. Reverse engineer, decompile, disassemble, or otherwise attempt to derive
 *    the source code of the software.
 * 3. Modify this license in any way, including but not limited to altering its
 *    terms, even by Advert Pulse or any other entity, without express written
 *    permission from Bloggios administrators. Bloggios is the creator of this
 *    license and retains exclusive rights to update or modify it.
 * 4. Update or modify the license without written permission from Bloggios
 *    administrators.
 * <p>
 * The software is provided "as is," and Advert Pulse makes no warranties,
 * express or implied, regarding the software, including but not limited to any
 * warranties of merchantability, fitness for a particular purpose, or
 * non-infringement.
 * <p>
 * For inquiries regarding licensing, please contact support@bloggios.com.
 */
package com.ap.listing.service.implementation;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: BuyServiceImplementation
 */

import com.ap.listing.dao.repository.TaskBuyerRepository;
import com.ap.listing.dao.repository.TaskPublisherRepository;
import com.ap.listing.dao.repository.WebsitePublisherRepository;
import com.ap.listing.enums.ErrorData;
import com.ap.listing.events.DemandEvent;
import com.ap.listing.exception.BadRequestException;
import com.ap.listing.feign.ApPaymentServiceFeignClient;
import com.ap.listing.model.TaskBuyer;
import com.ap.listing.model.TaskPublisher;
import com.ap.listing.model.WebsitePublisher;
import com.ap.listing.payload.records.DemandDataRecord;
import com.ap.listing.payload.request.BuyContentPlacementRequest;
import com.ap.listing.payload.request.BuyLinkInsertionRequest;
import com.ap.listing.payload.request.BuyWritingAndPlacement;
import com.ap.listing.payload.request.SendFundsToReservedRequest;
import com.ap.listing.payload.response.WalletResponse;
import com.ap.listing.processor.BuyContentPlacementNotificationProcessor;
import com.ap.listing.scheduler.generator.AutoTaskRejectScheduler;
import com.ap.listing.service.BuyService;
import com.ap.listing.transformer.PrepareTaskBuyContentPlacement;
import com.ap.listing.transformer.PrepareTaskBuyLinkInsertion;
import com.ap.listing.transformer.PrepareTasksBuyWritingAndPlacement;
import com.ap.listing.utils.ExtractTokenUtil;
import com.ap.listing.utils.SecurityContextUtil;
import com.ap.listing.validator.BuyContentPlacementRequestValidator;
import com.ap.listing.validator.BuyLinkInsertionRequestValidator;
import com.ap.listing.validator.BuyWritingAndPlacementRequestValidator;
import com.bloggios.provider.payload.ModuleResponse;
import com.bloggios.provider.utils.IpUtils;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@RequiredArgsConstructor
@Slf4j
public class BuyServiceImplementation implements BuyService {

    private final WebsitePublisherRepository websitePublisherRepository;
    private final BuyContentPlacementRequestValidator buyContentPlacementRequestValidator;
    private final PrepareTaskBuyContentPlacement prepareTaskBuyContentPlacement;
    private final TaskPublisherRepository taskPublisherRepository;
    private final TaskBuyerRepository taskBuyerRepository;
    private final ApPaymentServiceFeignClient apPaymentServiceFeignClient;
    private final ApplicationEventPublisher applicationEventPublisher;
    private final BuyContentPlacementNotificationProcessor buyContentPlacementNotificationProcessor;
    private final AutoTaskRejectScheduler autoTaskRejectScheduler;
    private final BuyLinkInsertionRequestValidator buyLinkInsertionRequestValidator;
    private final PrepareTaskBuyLinkInsertion prepareTaskBuyLinkInsertion;
    private final BuyWritingAndPlacementRequestValidator buyWritingAndPlacementRequestValidator;
    private final PrepareTasksBuyWritingAndPlacement prepareTasksBuyWritingAndPlacement;

    @Override
    @Transactional
    public ResponseEntity<ModuleResponse> buyContentPlacement(BuyContentPlacementRequest buyContentPlacementRequest, String publishingId, HttpServletRequest request) {
        WebsitePublisher websitePublisher = websitePublisherRepository.findByPublishingId(publishingId)
                .orElseThrow(() -> new BadRequestException(ErrorData.WEBSITE_PUBLISHER_NOT_FOUND));
        buyContentPlacementRequestValidator.validate(buyContentPlacementRequest, websitePublisher);
        WalletResponse walletResponse = getWalletForBuyer(request);
        TaskPublisher taskPublisher = prepareTaskBuyContentPlacement.preparePublisherTask(buyContentPlacementRequest, websitePublisher);
        if (taskPublisher.getTotalPrice() > walletResponse.getBalance()) {
            throw new BadRequestException(
                    ErrorData.INSUFFICIENT_FUNDS,
                    "Balance",
                    String.format("Your wallet has insufficient funds. Please add %s USD to place an order", (taskPublisher.getTotalPrice() - walletResponse.getBalance())));
        }
        TaskPublisher taskPublisherResponse = taskPublisherRepository.save(taskPublisher);
        log.info("Task publisher saved to database : {}", taskPublisherResponse);
        TaskBuyer taskBuyer = prepareTaskBuyContentPlacement.prepareBuyerTask(taskPublisherResponse);
        TaskBuyer taskBuyerResponse = taskBuyerRepository.save(taskBuyer);
        log.info("Task buyer saved to database : {}", taskBuyerResponse);
        ModuleResponse moduleResponse = apPaymentServiceFeignClient.sendFundsToReserved(
                ExtractTokenUtil.extractToken(request),
                new SendFundsToReservedRequest(
                        taskBuyerResponse.getTaskId(),
                        walletResponse.getWalletId(),
                        taskBuyerResponse.getTotalPrice(),
                        websitePublisher.getUserId()
                )
        );
        log.info("Response received from ap-payment-service for sendFundsToReserved: {}", moduleResponse);
        buyContentPlacementNotificationProcessor.process(taskPublisherResponse);
        log.info("Publishing Event >> DemandEvent");
        applicationEventPublisher.publishEvent(new DemandEvent(
                new DemandDataRecord(
                        IpUtils.getRemoteAddress(request),
                        taskBuyerResponse.getTaskId()
                )
        ));
        autoTaskRejectScheduler.process(taskPublisherResponse);
        return ResponseEntity.ok(
                ModuleResponse
                        .builder()
                        .message("Order Processed")
                        .userId(UUID.fromString(SecurityContextUtil.getLoggedInUserOrThrow().getUserId()))
                        .build()
        );
    }

    @Override
    @Transactional
    public ResponseEntity<ModuleResponse> buyWritingPlacement(BuyWritingAndPlacement buyWritingAndPlacement,String publishingId,HttpServletRequest httpServletRequest) {
        WebsitePublisher websitePublisher = websitePublisherRepository.findByPublishingId(publishingId)
                .orElseThrow(() -> new BadRequestException(ErrorData.WEBSITE_PUBLISHER_NOT_FOUND));
        buyWritingAndPlacementRequestValidator.validate(buyWritingAndPlacement,websitePublisher);
        WalletResponse walletResponse=apPaymentServiceFeignClient.getWallet(
                ExtractTokenUtil.extractToken(httpServletRequest),
                "BUYER"
        );
        log.info("Wallet response: {}", walletResponse);
        TaskPublisher taskPublisher=prepareTasksBuyWritingAndPlacement.preparePublisherTask(buyWritingAndPlacement,websitePublisher);
        if (taskPublisher.getTotalPrice() > walletResponse.getBalance()) {
            throw new BadRequestException(
                    ErrorData.INSUFFICIENT_FUNDS,
                    "Balance",
                    String.format("Your wallet has insufficient funds. Please add %s USD to place an order", (taskPublisher.getTotalPrice() - walletResponse.getBalance())));
        }
        TaskPublisher taskPublisherResponse = taskPublisherRepository.save(taskPublisher);
        log.info("Task publisher saved to database : {}", taskPublisherResponse);
        TaskBuyer taskBuyer = prepareTasksBuyWritingAndPlacement.prepareBuyerTask(taskPublisherResponse);
        TaskBuyer taskBuyerResponse = taskBuyerRepository.save(taskBuyer);
        log.info("Task buyer saved to database : {}", taskBuyerResponse);
        ModuleResponse moduleResponse = apPaymentServiceFeignClient.sendFundsToReserved(
                ExtractTokenUtil.extractToken(httpServletRequest),
                new SendFundsToReservedRequest(
                        taskBuyerResponse.getTaskId(),
                        walletResponse.getWalletId(),
                        taskBuyerResponse.getTotalPrice(),
                        websitePublisher.getUserId()
                )
        );
        log.info("Response received from ap-payment-service for sendFundsToReserved: {}", moduleResponse);
        return ResponseEntity.ok(
                ModuleResponse
                        .builder()
                        .message("Order Processed")
                        .userId(UUID.fromString(SecurityContextUtil.getLoggedInUserOrThrow().getUserId()))
                        .build()
        );

    }

    @Override
    @Transactional
    public ResponseEntity<ModuleResponse> buyLinkInsertion(BuyLinkInsertionRequest buyLinkInsertionRequest, String publishingId, HttpServletRequest request) {
        WebsitePublisher websitePublisher = websitePublisherRepository.findByPublishingId(publishingId)
                .orElseThrow(() -> new BadRequestException(ErrorData.WEBSITE_PUBLISHER_NOT_FOUND));

        if (websitePublisher.getUserId().equals(SecurityContextUtil.getLoggedInUser().getUserId()))
            throw new BadRequestException(ErrorData.CANNOT_PURCHASE_OWN_LISTED_ORDER);

        buyLinkInsertionRequestValidator.validate(buyLinkInsertionRequest);

        WalletResponse walletResponse = getWalletForBuyer(request);

        TaskPublisher taskPublisher = prepareTaskBuyLinkInsertion.preparePublisherTask(buyLinkInsertionRequest, websitePublisher);
        if (taskPublisher.getTotalPrice() > walletResponse.getBalance()) {
            throw new BadRequestException(
                    ErrorData.INSUFFICIENT_FUNDS,
                    "Balance",
                    String.format("Your wallet has insufficient funds. Please add %s USD to place an order", (taskPublisher.getTotalPrice() - walletResponse.getBalance())));
        }
        TaskPublisher taskPublisherResponse = taskPublisherRepository.save(taskPublisher);
        log.info("Task publisher saved to database : {}", taskPublisherResponse);
        TaskBuyer taskBuyer = prepareTaskBuyLinkInsertion.prepareBuyerTask(taskPublisherResponse);
        TaskBuyer taskBuyerResponse = taskBuyerRepository.save(taskBuyer);
        log.info("Task buyer saved to database : {}", taskBuyerResponse);
        ModuleResponse moduleResponse = apPaymentServiceFeignClient.sendFundsToReserved(
                ExtractTokenUtil.extractToken(request),
                new SendFundsToReservedRequest(
                        taskBuyerResponse.getTaskId(),
                        walletResponse.getWalletId(),
                        taskBuyerResponse.getTotalPrice(),
                        websitePublisher.getUserId()
                )
        );
        log.info("Response received from ap-payment-service for sendFundsToReserved: {}", moduleResponse);
        //buyContentPlacementNotificationProcessor.process(taskPublisherResponse);
        log.info("Publishing Event >> DemandEvent");
        applicationEventPublisher.publishEvent(new DemandEvent(
                new DemandDataRecord(
                        IpUtils.getRemoteAddress(request),
                        taskBuyerResponse.getTaskId()
                )
        ));
        autoTaskRejectScheduler.process(taskPublisherResponse);
        return ResponseEntity.ok(
                ModuleResponse
                        .builder()
                        .message("Order Processed")
                        .userId(UUID.fromString(SecurityContextUtil.getLoggedInUserOrThrow().getUserId()))
                        .build()
        );
    }

    private WalletResponse getWalletForBuyer(HttpServletRequest request) {
        WalletResponse walletResponse = apPaymentServiceFeignClient.getWallet(
                ExtractTokenUtil.extractToken(request),
                "BUYER"
        );
        log.info("Wallet response: {}", walletResponse);
        return  walletResponse;
    }
}
