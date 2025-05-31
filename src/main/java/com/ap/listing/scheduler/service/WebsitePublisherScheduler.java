package com.ap.listing.scheduler.service;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: TwoHourScheduler
 */

import com.ap.listing.constants.ServiceConstants;
import com.ap.listing.dao.repository.SchedulerRepository;
import com.ap.listing.enums.ScheduleTaskType;
import com.ap.listing.model.Scheduler;
import com.ap.listing.scheduler.processor.WebsitePublisherApprovalSchedulerProcessor;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.MDC;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@RequiredArgsConstructor
@Slf4j
public class WebsitePublisherScheduler {

    private final SchedulerRepository schedulerRepository;
    private final WebsitePublisherApprovalSchedulerProcessor websitePublisherApprovalSchedulerProcessor;

    public void doProcess() {
        try {
            List<Scheduler> listOfSchedulers = schedulerRepository.findAllByScheduledTaskTypeAndIsSchedulingDone(ScheduleTaskType.APPROVE_PUBLISHER, Boolean.FALSE);
            log.info("Found {} schedulers for Approve Publisher", listOfSchedulers.size());
            for (Scheduler scheduler : listOfSchedulers) {
                MDC.put(ServiceConstants.SCHEDULER_ID, scheduler.getSchedulerId());
                websitePublisherApprovalSchedulerProcessor.process(scheduler);
            }
        } finally {
            MDC.remove(ServiceConstants.SCHEDULER_ID);
        }
    }
}
