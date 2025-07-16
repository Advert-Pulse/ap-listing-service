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
package com.ap.listing.processor;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: PublisherRejectedTaskProcessor
 */

import com.ap.listing.dao.repository.TaskBuyerRepository;
import com.ap.listing.dao.repository.TaskPublisherRepository;
import com.ap.listing.enums.BuyerTaskStatus;
import com.ap.listing.enums.PublisherTaskStatus;
import com.ap.listing.feign.ApPaymentServiceFeignClient;
import com.ap.listing.model.TaskBuyer;
import com.ap.listing.model.TaskPublisher;
import com.ap.listing.payload.BuyerTaskStatusPayload;
import com.ap.listing.payload.PublisherTaskStatusPayload;
import com.ap.listing.payload.request.ReverseReservedFundsRequest;
import com.ap.listing.utils.ExtractTokenUtil;
import com.bloggios.provider.payload.ModuleResponse;
import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.Date;
import java.util.List;

@Component
@RequiredArgsConstructor
@Slf4j
public class PublisherRejectedTaskProcessor {

    private final ApPaymentServiceFeignClient apPaymentServiceFeignClient;
    private final TaskBuyerRepository taskBuyerRepository;
    private final TaskPublisherRepository taskPublisherRepository;

    @Value("${feign-client.ap-payment-service.ap-payment-internal-key}")
    private String apInternalPaymentKey;

    public void process(TaskPublisher taskPublisher, TaskBuyer taskBuyer, HttpServletRequest request) {
        log.info("Processing publisher rejected task taskPublisher: {}, taskBuyer: {}", taskPublisher, taskBuyer);
        ModuleResponse reverseReservedFundResponse = apPaymentServiceFeignClient.reverseReservedFund(
                ExtractTokenUtil.extractToken(request),
                new ReverseReservedFundsRequest(
                        taskBuyer.getBuyerId(),
                        taskPublisher.getPublisherId(),
                        taskPublisher.getTotalPrice(),
                        taskPublisher.getTaskId()
                )
        );
        Date now = new Date();
        updateBuyerTask(taskBuyer, now);
        updatePublisher(taskPublisher, now);
        log.info("{} process completed", getClass().getSimpleName());
    }

    public void process(TaskPublisher taskPublisher, TaskBuyer taskBuyer) {
        log.info("Processing publisher rejected task taskPublisher: {}, taskBuyer: {}", taskPublisher, taskBuyer);
        ModuleResponse reverseReservedFundResponse = apPaymentServiceFeignClient.reverseReservedFundInternal(
                apInternalPaymentKey,
                new ReverseReservedFundsRequest(
                        taskBuyer.getBuyerId(),
                        taskPublisher.getPublisherId(),
                        taskPublisher.getTotalPrice(),
                        taskPublisher.getTaskId()
                )
        );
        Date now = new Date();
        updateBuyerTask(taskBuyer, now);
        updatePublisher(taskPublisher, now);
        log.info("{} process completed", getClass().getSimpleName());
    }

    private void updateBuyerTask(TaskBuyer taskBuyer, Date now) {
        taskBuyer.setCurrentStatus(BuyerTaskStatus.REJECTED.name());
        List<BuyerTaskStatusPayload> taskStatus = taskBuyer.getTaskStatus();
        taskStatus.add(new BuyerTaskStatusPayload(now, BuyerTaskStatus.REJECTED));
        taskBuyer.setTaskStatus(taskStatus);
        taskBuyer.setDateUpdated(now);
        TaskBuyer taskBuyerResponse = taskBuyerRepository.save(taskBuyer);
        log.info("Task buyer successfully updated to Database: {}", taskBuyerResponse.toString());
    }

    private void updatePublisher(TaskPublisher taskPublisher, Date now) {
        taskPublisher.setCurrentStatus(PublisherTaskStatus.REJECTED.name());
        List<PublisherTaskStatusPayload> taskStatus = taskPublisher.getTaskStatus();
        taskStatus.add(new PublisherTaskStatusPayload(now, PublisherTaskStatus.REJECTED));
        taskPublisher.setTaskStatus(taskStatus);
        taskPublisher.setDateUpdated(now);
        TaskPublisher taskPublisherResponse = taskPublisherRepository.save(taskPublisher);
        log.info("Task publisher successfully updated to Database : {}", taskPublisherResponse.toString());
    }
}
