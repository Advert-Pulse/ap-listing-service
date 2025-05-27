package com.ap.listing.service.implementation;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: PublisherServiceImplementation
 */

import com.ap.listing.dao.repository.TaskBuyerRepository;
import com.ap.listing.dao.repository.TaskPublisherRepository;
import com.ap.listing.enums.BuyerTaskStatus;
import com.ap.listing.enums.ErrorData;
import com.ap.listing.enums.PublisherTaskStatus;
import com.ap.listing.exception.BadRequestException;
import com.ap.listing.model.TaskBuyer;
import com.ap.listing.model.TaskPublisher;
import com.ap.listing.payload.BuyerTaskStatusPayload;
import com.ap.listing.payload.PublisherTaskStatusPayload;
import com.ap.listing.service.PublisherService;
import com.ap.listing.utils.SecurityContextUtil;
import com.bloggios.provider.payload.ModuleResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import javax.swing.tree.DefaultTreeCellEditor;
import java.util.Date;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
@Slf4j
public class PublisherServiceImplementation implements PublisherService {

    private final TaskPublisherRepository taskPublisherRepository;
    private final TaskBuyerRepository taskBuyerRepository;

    @Override
    public ResponseEntity<ModuleResponse> manageTaskInitial(String taskId, String status) {
        TaskPublisher taskPublisher = taskPublisherRepository.findByTaskId(taskId)
                .orElseThrow(() -> new BadRequestException(ErrorData.TASK_PUBLISHER_NOT_FOUND));
        if (!taskPublisher.getPublisherId().equalsIgnoreCase(SecurityContextUtil.getLoggedInUserOrThrow().getUserId())) {
            throw new BadRequestException(ErrorData.CANNOT_MANAGE_OTHERS_TASK);
        }
        Date now = new Date();
        TaskBuyer taskBuyer = taskBuyerRepository.findByTaskId(taskId)
                .orElseThrow(() -> new BadRequestException(ErrorData.TASK_BUYER_NOT_FOUND));
        if (status.equalsIgnoreCase(PublisherTaskStatus.IN_PROGRESS.name())) {
            updatePublisherTaskForManageTaskInitial(taskPublisher, now);
            updateBuyerTaskForManageTaskInitial(taskBuyer, now);
        } else if (status.equalsIgnoreCase(PublisherTaskStatus.REJECTED.name())) {

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
}
