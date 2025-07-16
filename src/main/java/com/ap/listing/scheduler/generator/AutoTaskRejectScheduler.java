package com.ap.listing.scheduler.generator;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: AutoTaskRejectScheduler
 */

import com.ap.listing.dao.repository.SchedulerRepository;
import com.ap.listing.enums.ScheduleTaskType;
import com.ap.listing.model.Scheduler;
import com.ap.listing.model.TaskPublisher;
import com.bloggios.provider.utils.DateUtils;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.util.Date;

@Component
@RequiredArgsConstructor
@Slf4j
public class AutoTaskRejectScheduler {

    private final SchedulerRepository schedulerRepository;

    @Value("${scheduler.autoRejectTask.scheduleAfter}")
    private int scheduleAfter;

    public void process(TaskPublisher taskPublisher) {
        Date now = new Date();
        Scheduler scheduler = Scheduler
                .builder()
                .primaryId(taskPublisher.getTaskPublisherUUID())
                .isSchedulingDone(false)
                .scheduledTaskType(ScheduleTaskType.AUTO_REJECT_TASK)
                .scheduledOn(DateUtils.addDays(now, scheduleAfter))
                .timesUsed(0)
                .createdOn(now)
                .updatedOn(now)
                .build();
        log.info("Scheduler Data Prepared for Task Publisher UUID: {}", taskPublisher.getTaskPublisherUUID());
        schedulerRepository.saveAndFlush(scheduler);
    }
}
