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
package com.ap.listing.scheduler;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: GlobalScheduler
 */

import com.ap.listing.scheduler.service.FetchWebsiteDataScheduler;
import com.ap.listing.scheduler.service.WebsitePublisherScheduler;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.util.Date;

@Component
@RequiredArgsConstructor
@Slf4j
public class GlobalScheduler {

    private final FetchWebsiteDataScheduler fetchWebsiteDataScheduler;
    private final WebsitePublisherScheduler websitePublisherScheduler;

//    @Scheduled(cron = "0 0 * * * ?")
    @Scheduled(cron = "0 */2 * * * ?")
    public void taskFetchWebsite() {
        long startTime = System.currentTimeMillis();
        log.info("Fetch Website scheduled task ran at {}", new Date());
        fetchWebsiteDataScheduler.doProcess();
        log.info("Fetch Website Scheduler took {} ms", System.currentTimeMillis() - startTime);
    }

    @Scheduled(fixedRate = 5400000)
    public void taskApproveWebsite() {
        long startTime = System.currentTimeMillis();
        log.info("Approve Website scheduled task ran at {}", new Date());
        websitePublisherScheduler.doProcess();
        log.info("Approve Website Scheduler took {} ms", System.currentTimeMillis() - startTime);
    }
}
