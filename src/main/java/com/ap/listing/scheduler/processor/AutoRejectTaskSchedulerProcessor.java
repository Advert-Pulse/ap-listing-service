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
package com.ap.listing.scheduler.processor;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: AutoRejectTaskSchedulerProcessor
 */

import com.ap.listing.dao.repository.SchedulerRepository;
import com.ap.listing.dao.repository.TaskPublisherRepository;
import com.ap.listing.enums.PublisherTaskStatus;
import com.ap.listing.model.Scheduler;
import com.ap.listing.model.TaskPublisher;
import com.ap.listing.service.PublisherService;
import com.ap.listing.utils.MessageUtil;
import com.bloggios.provider.payload.ModuleResponse;
import com.bloggios.provider.utils.DateUtils;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;

import java.util.Date;
import java.util.Optional;

@Component
@RequiredArgsConstructor
@Slf4j
public class AutoRejectTaskSchedulerProcessor {

    @Value("${scheduler.autoRejectTask.limit}")
    private int limit;

    @Value("${scheduler.autoRejectTask.multiplier}")
    private int multiplier;

    private final TaskPublisherRepository taskPublisherRepository;
    private final SchedulerRepository schedulerRepository;
    private final PublisherService publisherService;

    public void process(Scheduler scheduler) {
        log.info("{} >> {}", getClass().getSimpleName(), scheduler);

        String taskUUID = scheduler.getPrimaryId();
        Optional<TaskPublisher> taskOptional = taskPublisherRepository.findById(taskUUID);
        Date now = new Date();

        if (taskOptional.isEmpty()) {
            completeScheduling(scheduler, now, "Task not found for primary id: " + taskUUID);
            return;
        }

        TaskPublisher taskPublisher = taskOptional.get();
        if (!PublisherTaskStatus.YOUR_ACCEPTANCE.name().equalsIgnoreCase(taskPublisher.getCurrentStatus())) {
            completeScheduling(scheduler, now, "Task is not in 'Your Acceptance' state: " + taskPublisher.getTaskId());
            return;
        }

        ResponseEntity<ModuleResponse> response = publisherService.manageTaskInitialInternal(
                taskPublisher.getTaskId(), PublisherTaskStatus.REJECTED.name());

        if (response.getStatusCode().is2xxSuccessful()) {
            markSchedulerSuccess(scheduler, now);
        } else {
            reschedule(scheduler, now);
        }
    }

    private void reschedule(Scheduler scheduler, Date now) {
        int timesUsed = scheduler.getTimesUsed();
        scheduler.setUpdatedOn(now);

        if (timesUsed < limit) {
            log.info("Rescheduling task - attempt {}", timesUsed + 1);
            scheduler.setTimesUsed(timesUsed + 1);
            scheduler.setScheduledOn(DateUtils.addHours(now, timesUsed * multiplier));
        } else {
            log.info("Rescheduling limit reached. Marking as failed.");
            scheduler.setIsSchedulingDone(true);
            scheduler.setIsPassed(false);
            scheduler.setScheduleCompletedOn(now);
        }

        Scheduler saved = schedulerRepository.save(scheduler);
        log.info("Scheduler saved to DB: {}", saved);
    }

    private void markSchedulerSuccess(Scheduler scheduler, Date now) {
        scheduler.setIsSchedulingDone(true);
        scheduler.setIsPassed(true);
        scheduler.setScheduleCompletedOn(now);
        scheduler.setUpdatedOn(now);

        Scheduler saved = schedulerRepository.saveAndFlush(scheduler);
        log.info("Scheduler marked successful: {}", saved);
    }

    private void completeScheduling(Scheduler scheduler, Date now, String message) {
        scheduler.setIsSchedulingDone(true);
        scheduler.setIsPassed(false);
        scheduler.setScheduleCompletedOn(now);
        scheduler.setUpdatedOn(now);
        scheduler.setMessage(MessageUtil.getMessage(scheduler.getMessage(), message));

        Scheduler saved = schedulerRepository.saveAndFlush(scheduler);
        log.info("Scheduler completed with message: {}", saved);
    }
}