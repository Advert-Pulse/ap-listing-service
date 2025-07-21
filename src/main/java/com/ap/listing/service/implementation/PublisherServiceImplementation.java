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
  File: PublisherServiceImplementation
 */

import com.ap.listing.dao.repository.BuyerImprovementRepository;
import com.ap.listing.dao.repository.TaskBuyerRepository;
import com.ap.listing.dao.repository.TaskPublisherRepository;
import com.ap.listing.enums.BuyerImprovementStatus;
import com.ap.listing.enums.BuyerTaskStatus;
import com.ap.listing.enums.ErrorData;
import com.ap.listing.enums.PublisherTaskStatus;
import com.ap.listing.exception.BadRequestException;
import com.ap.listing.model.BuyerImprovement;
import com.ap.listing.model.TaskBuyer;
import com.ap.listing.model.TaskPublisher;
import com.ap.listing.payload.BuyerTaskStatusPayload;
import com.ap.listing.payload.PublisherTaskStatusPayload;
import com.ap.listing.payload.request.PublisherInitialApprovalRequest;
import com.ap.listing.processor.PublisherRejectedTaskProcessor;
import com.ap.listing.service.PublisherService;
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
public class PublisherServiceImplementation implements PublisherService {

    private final TaskPublisherRepository taskPublisherRepository;
    private final TaskBuyerRepository taskBuyerRepository;
    private final PublisherRejectedTaskProcessor publisherRejectedTaskProcessor;
    private final BuyerImprovementRepository buyerImprovementRepository;

    @Override
    public ResponseEntity<ModuleResponse> manageTaskInitial(String taskId, String status, HttpServletRequest httpServletRequest) {
        TaskPublisher taskPublisher = taskPublisherRepository.findByTaskId(taskId)
                .orElseThrow(() -> new BadRequestException(ErrorData.TASK_PUBLISHER_NOT_FOUND));
        if (!taskPublisher.getPublisherId().equalsIgnoreCase(SecurityContextUtil.getLoggedInUserOrThrow().getUserId())) {
            throw new BadRequestException(ErrorData.CANNOT_MANAGE_OTHERS_TASK);
        }
        Date now = new Date();
        TaskBuyer taskBuyer = taskBuyerRepository.findByTaskId(taskId)
                .orElseThrow(() -> new BadRequestException(ErrorData.TASK_BUYER_NOT_FOUND));
        if (!taskPublisher.getCurrentStatus().equalsIgnoreCase(PublisherTaskStatus.YOUR_ACCEPTANCE.name())) {
            throw new BadRequestException(ErrorData.TASK_SHOULD_BE_IN_YOUR_ACCEPTANCE_TO_MANAGE_IT_INITIALLY);
        }
        if (status.equalsIgnoreCase(PublisherTaskStatus.IN_PROGRESS.name())) {
            updatePublisherTaskForManageTaskInitial(taskPublisher, now);
            updateBuyerTaskForManageTaskInitial(taskBuyer, now);
        } else if (status.equalsIgnoreCase(PublisherTaskStatus.REJECTED.name())) {
            publisherRejectedTaskProcessor.process(taskPublisher, taskBuyer, httpServletRequest);
        } else {
            throw new BadRequestException(
                    ErrorData.MANAGE_TASK_INITIAL_STATUS_INVALID,
                    "status",
                    String.format("You cannot choose %s status for Manage Task Initial", status)
            );
        }
        String toUpdateStatus = status.equalsIgnoreCase(PublisherTaskStatus.IN_PROGRESS.name()) ?
                "In Progress" :
                "Rejected";
        return ResponseEntity.ok(ModuleResponse
                .builder()
                .message("Status has been updated to " + toUpdateStatus)
                .userId(UUID.fromString(SecurityContextUtil.getLoggedInUserOrThrow().getUserId()))
                .build());
    }

    @Override
    public ResponseEntity<ModuleResponse> manageTaskInitialInternal(String taskId, String status) {
        TaskPublisher taskPublisher = taskPublisherRepository.findByTaskId(taskId)
                .orElseThrow(() -> new BadRequestException(ErrorData.TASK_PUBLISHER_NOT_FOUND));
        if (!taskPublisher.getPublisherId().equalsIgnoreCase(SecurityContextUtil.getLoggedInUserOrThrow().getUserId())) {
            throw new BadRequestException(ErrorData.CANNOT_MANAGE_OTHERS_TASK);
        }
        Date now = new Date();
        TaskBuyer taskBuyer = taskBuyerRepository.findByTaskId(taskId)
                .orElseThrow(() -> new BadRequestException(ErrorData.TASK_BUYER_NOT_FOUND));
        if (!taskPublisher.getCurrentStatus().equalsIgnoreCase(PublisherTaskStatus.YOUR_ACCEPTANCE.name())) {
            throw new BadRequestException(ErrorData.TASK_SHOULD_BE_IN_YOUR_ACCEPTANCE_TO_MANAGE_IT_INITIALLY);
        }
        if (status.equalsIgnoreCase(PublisherTaskStatus.IN_PROGRESS.name())) {
            updatePublisherTaskForManageTaskInitial(taskPublisher, now);
            updateBuyerTaskForManageTaskInitial(taskBuyer, now);
        } else if (status.equalsIgnoreCase(PublisherTaskStatus.REJECTED.name())) {
            publisherRejectedTaskProcessor.process(taskPublisher, taskBuyer);
        } else {
            throw new BadRequestException(
                    ErrorData.MANAGE_TASK_INITIAL_STATUS_INVALID,
                    "status",
                    String.format("You cannot choose %s status for Manage Task Initial", status)
            );
        }
        String toUpdateStatus = status.equalsIgnoreCase(PublisherTaskStatus.IN_PROGRESS.name()) ?
                "In Progress" :
                "Rejected";
        return ResponseEntity.ok(ModuleResponse
                .builder()
                .message("Status has been updated to " + toUpdateStatus)
                .userId(UUID.fromString(SecurityContextUtil.getLoggedInUserOrThrow().getUserId()))
                .build());
    }

    @Override
    public ResponseEntity<ModuleResponse> initialApproval(String taskId, PublisherInitialApprovalRequest publisherInitialApprovalRequest) {
        TaskPublisher taskPublisher = taskPublisherRepository.findByTaskId(taskId)
                .orElseThrow(() -> new BadRequestException(ErrorData.TASK_PUBLISHER_NOT_FOUND));
        if (!taskPublisher.getPublisherId().equalsIgnoreCase(SecurityContextUtil.getLoggedInUserOrThrow().getUserId())) {
            throw new BadRequestException(ErrorData.CANNOT_MANAGE_OTHERS_TASK);
        }
        Date now = new Date();
        TaskBuyer taskBuyer = taskBuyerRepository.findByTaskId(taskId)
                .orElseThrow(() -> new BadRequestException(ErrorData.TASK_BUYER_NOT_FOUND));
        if (!(taskPublisher.getCurrentStatus().equalsIgnoreCase(PublisherTaskStatus.IN_PROGRESS.name()) || taskPublisher.getCurrentStatus().equalsIgnoreCase(PublisherTaskStatus.IMPROVEMENT.name()))) {
            throw new BadRequestException(ErrorData.TASK_SHOULD_BE_IN_IN_PROGRESS_OR_IMPROVEMENT);
        }
        if (taskBuyer.getCurrentStatus().equalsIgnoreCase(PublisherTaskStatus.IMPROVEMENT.name())) {
            buyerImprovementRepository.findByTaskIdAndBuyerImprovementStatus(taskPublisher.getTaskId(), BuyerImprovementStatus.PENDING)
                    .ifPresent(buyerImprovement -> {
                        buyerImprovement.setDateUpdated(now);
                        buyerImprovement.setBuyerImprovementStatus(BuyerImprovementStatus.COMPLETED);
                        BuyerImprovement buyerImprovementStatus = buyerImprovementRepository.save(buyerImprovement);
                        log.info("Updating buyer improvement: {}", buyerImprovementStatus.toString());
                    });
        }
        updateBuyerTaskForInitialApproval(taskBuyer, now, publisherInitialApprovalRequest);
        updatePublisherTaskForInitialApproval(taskPublisher, now, publisherInitialApprovalRequest);
        return ResponseEntity.ok(ModuleResponse
                .builder()
                .message("Status has been updated to Buyer Approval")
                .userId(UUID.fromString(SecurityContextUtil.getLoggedInUserOrThrow().getUserId()))
                .build());
    }

    private void updateBuyerTaskForManageTaskInitial(TaskBuyer taskBuyer, Date now) {
        taskBuyer.setCurrentStatus(BuyerTaskStatus.IN_PROGRESS.name());
        List<BuyerTaskStatusPayload> taskStatus = taskBuyer.getTaskStatus();
        taskStatus.add(new BuyerTaskStatusPayload(now, BuyerTaskStatus.IN_PROGRESS));
        taskBuyer.setTaskStatus(taskStatus);
        taskBuyer.setDateUpdated(now);
        TaskBuyer taskBuyerResponse = taskBuyerRepository.save(taskBuyer);
        log.info("Task buyer created: {}", taskBuyerResponse.toString());
    }

    private void updatePublisherTaskForManageTaskInitial(TaskPublisher taskPublisher, Date now) {
        taskPublisher.setCurrentStatus(PublisherTaskStatus.IN_PROGRESS.name());
        List<PublisherTaskStatusPayload> taskStatus = taskPublisher.getTaskStatus();
        taskStatus.add(new PublisherTaskStatusPayload(now, PublisherTaskStatus.IN_PROGRESS));
        taskPublisher.setTaskStatus(taskStatus);
        taskPublisher.setDateUpdated(now);
        TaskPublisher taskPublisherResponse = taskPublisherRepository.save(taskPublisher);
        log.info("Task publisher successfully updated to Database : {}", taskPublisherResponse.toString());
    }

    private void updateBuyerTaskForInitialApproval(TaskBuyer taskBuyer, Date now, PublisherInitialApprovalRequest publisherInitialApprovalRequest) {
        taskBuyer.setCurrentStatus(BuyerTaskStatus.YOUR_APPROVAL.name());
        List<BuyerTaskStatusPayload> taskStatus = taskBuyer.getTaskStatus();
        taskStatus.add(new BuyerTaskStatusPayload(now, BuyerTaskStatus.YOUR_APPROVAL));
        taskBuyer.setTaskStatus(taskStatus);
        taskBuyer.setDateUpdated(now);
        taskBuyer.setTaskPlacementUrl(publisherInitialApprovalRequest.getPublishedLink());
        TaskBuyer taskBuyerResponse = taskBuyerRepository.save(taskBuyer);
        log.info("Task buyer created: {}", taskBuyerResponse.toString());
    }

    private void updatePublisherTaskForInitialApproval(TaskPublisher taskPublisher, Date now, PublisherInitialApprovalRequest publisherInitialApprovalRequest) {
        taskPublisher.setCurrentStatus(PublisherTaskStatus.BUYER_APPROVAL.name());
        List<PublisherTaskStatusPayload> taskStatus = taskPublisher.getTaskStatus();
        taskStatus.add(new PublisherTaskStatusPayload(now, PublisherTaskStatus.BUYER_APPROVAL));
        taskPublisher.setTaskStatus(taskStatus);
        taskPublisher.setDateUpdated(now);
        taskPublisher.setTaskPlacementUrl(publisherInitialApprovalRequest.getPublishedLink());
        TaskPublisher taskPublisherResponse = taskPublisherRepository.save(taskPublisher);
        log.info("Task publisher successfully updated to Database : {}", taskPublisherResponse.toString());
    }
}
