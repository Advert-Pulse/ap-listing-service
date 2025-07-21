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
  File: BuyContentPlacementNotificationProcessor
 */

import com.ap.listing.model.TaskPublisher;
import com.ap.listing.payload.kafka.BuyContentPlacementEvent;
import com.ap.listing.processor.kafka.BuyContentPlacementNotificationProducer;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
@Slf4j
public class BuyContentPlacementNotificationProcessor {

    private final BuyContentPlacementNotificationProducer buyContentPlacementNotificationProducer;

    public void process(TaskPublisher taskPublisherResponse) {
        log.info("{} >> process -> taskPublisherResponse: {}", taskPublisherResponse.getClass().getSimpleName(), taskPublisherResponse);
        BuyContentPlacementEvent buyContentPlacementEvent = BuyContentPlacementEvent
                .builder()
                .buyerId(taskPublisherResponse.getBuyerId())
                .publisherId(taskPublisherResponse.getPublisherId())
                .taskId(taskPublisherResponse.getTaskId())
                .domain(taskPublisherResponse.getSiteUrl())
                .publishingId(taskPublisherResponse.getPublishingId())
                .build();
        buyContentPlacementNotificationProducer.sendMessage(buyContentPlacementEvent);
    }
}
