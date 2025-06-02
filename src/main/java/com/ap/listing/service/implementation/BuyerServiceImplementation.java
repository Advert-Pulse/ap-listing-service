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
  File: BuyerServiceImplementation
 */

import com.ap.listing.dao.repository.BuyerImprovementRepository;
import com.ap.listing.dao.repository.TaskBuyerRepository;
import com.ap.listing.dao.repository.TaskPublisherRepository;
import com.ap.listing.enums.BuyerTaskStatus;
import com.ap.listing.enums.ErrorData;
import com.ap.listing.enums.PublisherTaskStatus;
import com.ap.listing.exception.BadRequestException;
import com.ap.listing.feign.ApPaymentServiceFeignClient;
import com.ap.listing.model.BuyerImprovement;
import com.ap.listing.model.TaskBuyer;
import com.ap.listing.model.TaskPublisher;
import com.ap.listing.payload.BuyerTaskStatusPayload;
import com.ap.listing.payload.PublisherTaskStatusPayload;
import com.ap.listing.payload.request.BuyerImprovementRequest;
import com.ap.listing.payload.request.ReserveFundToBalanceRequest;
import com.ap.listing.service.BuyerService;
import com.ap.listing.transformer.BuyerImprovementRequestToEntityTransformer;
import com.ap.listing.utils.ExtractTokenUtil;
import com.ap.listing.utils.SecurityContextUtil;
import com.bloggios.provider.payload.ModuleResponse;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
@Slf4j
public class BuyerServiceImplementation implements BuyerService {

    private final TaskBuyerRepository taskBuyerRepository;
    private final TaskPublisherRepository taskPublisherRepository;
    private final BuyerImprovementRequestToEntityTransformer buyerImprovementRequestToEntityTransformer;
    private final BuyerImprovementRepository buyerImprovementRepository;
    private final ApPaymentServiceFeignClient apPaymentServiceFeignClient;

    @Override
    public ResponseEntity<ModuleResponse> manageImprovement(BuyerImprovementRequest buyerImprovementRequest) {
        TaskBuyer taskBuyer = taskBuyerRepository.findByTaskId(buyerImprovementRequest.getTaskId())
                .orElseThrow(() -> new BadRequestException(ErrorData.TASK_BUYER_NOT_FOUND));
        if (!taskBuyer.getBuyerId().equalsIgnoreCase(SecurityContextUtil.getLoggedInUserOrThrow().getUserId())) {
            throw new BadRequestException(ErrorData.CANNOT_MANAGE_OTHERS_TASK);
        }
        if (!taskBuyer.getCurrentStatus().equalsIgnoreCase(BuyerTaskStatus.YOUR_APPROVAL.name())) {
            throw new BadRequestException(ErrorData.TASK_SHOULD_BE_BUYER_APPROVAL_TO_MANAGE);
        }
        TaskPublisher taskPublisher = taskPublisherRepository.findByTaskId(buyerImprovementRequest.getTaskId())
                .orElseThrow(() -> new BadRequestException(ErrorData.TASK_PUBLISHER_NOT_FOUND));
        BuyerImprovement buyerImprovement = buyerImprovementRequestToEntityTransformer.transform(buyerImprovementRequest, taskBuyer);
        BuyerImprovement buyerImprovementResponse = buyerImprovementRepository.save(buyerImprovement);
        log.info("Buyer Improvement saved to database : {}", buyerImprovementResponse.toString());
        Date now = new Date();
        updateBuyerTaskForManageImprovement(taskBuyer, now);
        updatePublisherTaskForManageImprovement(taskPublisher, now);
        return ResponseEntity.ok(
                ModuleResponse
                        .builder()
                        .message("Task status updated to Improvement")
                        .userId(UUID.fromString(SecurityContextUtil.getLoggedInUserOrThrow().getUserId()))
                        .build()
        );
    }

    @Override
    public ResponseEntity<ModuleResponse> manageCompleted(String taskId, HttpServletRequest request) {
        TaskBuyer taskBuyer = taskBuyerRepository.findByTaskId(taskId)
                .orElseThrow(() -> new BadRequestException(ErrorData.TASK_BUYER_NOT_FOUND));
        if (!taskBuyer.getBuyerId().equalsIgnoreCase(SecurityContextUtil.getLoggedInUserOrThrow().getUserId())) {
            throw new BadRequestException(ErrorData.CANNOT_MANAGE_OTHERS_TASK);
        }
        if (!taskBuyer.getCurrentStatus().equalsIgnoreCase(BuyerTaskStatus.YOUR_APPROVAL.name())) {
            throw new BadRequestException(ErrorData.TASK_SHOULD_BE_BUYER_APPROVAL_TO_MANAGE);
        }
        TaskPublisher taskPublisher = taskPublisherRepository.findByTaskId(taskId)
                .orElseThrow(() -> new BadRequestException(ErrorData.TASK_PUBLISHER_NOT_FOUND));
        ModuleResponse reserveFundsToBalanceResponse = apPaymentServiceFeignClient.reserveFundsToBalance(
                ExtractTokenUtil.extractToken(request),
                ReserveFundToBalanceRequest
                        .builder()
                        .buyerId(taskBuyer.getBuyerId())
                        .publisherId(taskBuyer.getPublisherId())
                        .amount(taskPublisher.getTotalPrice() - taskPublisher.getPlatformFee())
                        .taskId(taskPublisher.getTaskId())
                        .platformFee(taskPublisher.getPlatformFee())
                        .build()
        );
        Date now = new Date();
        updateBuyerTaskForManageCompleted(taskBuyer, now);
        updatePublisherTaskForManageCompleted(taskPublisher, now);
        return ResponseEntity.ok(
                ModuleResponse
                        .builder()
                        .message("Task Completed")
                        .userId(UUID.fromString(SecurityContextUtil.getLoggedInUserOrThrow().getUserId()))
                        .build()
        );
    }

    private void updateBuyerTaskForManageImprovement(TaskBuyer taskBuyer, Date now) {
        taskBuyer.setCurrentStatus(BuyerTaskStatus.IMPROVEMENT.name());
        List<BuyerTaskStatusPayload> taskStatus = taskBuyer.getTaskStatus();
        taskStatus.add(new BuyerTaskStatusPayload(now, BuyerTaskStatus.IMPROVEMENT));
        taskBuyer.setTaskStatus(taskStatus);
        taskBuyer.setDateUpdated(now);
        TaskBuyer taskBuyerResponse = taskBuyerRepository.save(taskBuyer);
        log.info("Task buyer created: {}", taskBuyerResponse.toString());
    }

    private void updatePublisherTaskForManageImprovement(TaskPublisher taskPublisher, Date now) {
        taskPublisher.setCurrentStatus(PublisherTaskStatus.IMPROVEMENT.name());
        List<PublisherTaskStatusPayload> taskStatus = taskPublisher.getTaskStatus();
        taskStatus.add(new PublisherTaskStatusPayload(now, PublisherTaskStatus.IMPROVEMENT));
        taskPublisher.setTaskStatus(taskStatus);
        taskPublisher.setDateUpdated(now);
        TaskPublisher taskPublisherResponse = taskPublisherRepository.save(taskPublisher);
        log.info("Task publisher successfully updated to Database : {}", taskPublisherResponse.toString());
    }

    private void updateBuyerTaskForManageCompleted(TaskBuyer taskBuyer, Date now) {
        taskBuyer.setCurrentStatus(BuyerTaskStatus.COMPLETED.name());
        List<BuyerTaskStatusPayload> taskStatus = taskBuyer.getTaskStatus();
        taskStatus.add(new BuyerTaskStatusPayload(now, BuyerTaskStatus.COMPLETED));
        taskBuyer.setTaskStatus(taskStatus);
        taskBuyer.setDateUpdated(now);
        TaskBuyer taskBuyerResponse = taskBuyerRepository.save(taskBuyer);
        log.info("Task buyer created: {}", taskBuyerResponse.toString());
    }

    private void updatePublisherTaskForManageCompleted(TaskPublisher taskPublisher, Date now) {
        taskPublisher.setCurrentStatus(PublisherTaskStatus.COMPLETED.name());
        List<PublisherTaskStatusPayload> taskStatus = taskPublisher.getTaskStatus();
        taskStatus.add(new PublisherTaskStatusPayload(now, PublisherTaskStatus.COMPLETED));
        taskPublisher.setTaskStatus(taskStatus);
        taskPublisher.setDateUpdated(now);
        TaskPublisher taskPublisherResponse = taskPublisherRepository.save(taskPublisher);
        log.info("Task publisher successfully updated to Database : {}", taskPublisherResponse.toString());
    }
}
