package com.ap.listing.scheduler.generator;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: WebsitePublisherSchedulerGenerator
 */

import com.ap.listing.dao.repository.SchedulerRepository;
import com.ap.listing.enums.ScheduleTaskType;
import com.ap.listing.enums.WebsitePublishingStatus;
import com.ap.listing.model.Scheduler;
import com.ap.listing.model.WebsitePublisher;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.Date;
import java.util.List;

@Component
@RequiredArgsConstructor
@Slf4j
public class WebsitePublisherSchedulerGenerator {


    private final SchedulerRepository schedulerRepository;

    public void process(WebsitePublisher websitePublisher) {
        if (websitePublisher.getWebsitePublishingStatus().equalsIgnoreCase(WebsitePublishingStatus.PENDING_MODERATION.name())) {
            List<Scheduler> byPrimaryId = schedulerRepository.findByPrimaryId(websitePublisher.getWebsitePublisherId());
            if (!CollectionUtils.isEmpty(byPrimaryId)) {
                byPrimaryId.forEach(schedulerRepository::delete);
            }
            Date now = new Date();
            Scheduler scheduler = Scheduler
                    .builder()
                    .primaryId(websitePublisher.getWebsitePublisherId())
                    .isSchedulingDone(false)
                    .scheduledTaskType(ScheduleTaskType.APPROVE_PUBLISHER)
                    .timesUsed(0)
                    .createdOn(now)
                    .updatedOn(now)
                    .build();
            log.info("Scheduler Data Prepared for Website Publisher Id: {}", websitePublisher.getWebsitePublisherId());
            schedulerRepository.saveAndFlush(scheduler);
        }
    }
}
