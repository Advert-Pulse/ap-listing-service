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
                    .scheduledOn(new Date())
                    .createdOn(now)
                    .updatedOn(now)
                    .build();
            log.info("Scheduler Data Prepared for Website Publisher Id: {}", websitePublisher.getWebsitePublisherId());
            schedulerRepository.saveAndFlush(scheduler);
        }
    }
}
