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
package com.ap.listing.utils;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: PublishingIdGenerator
 */

import com.ap.listing.dao.repository.WebsitePublisherRepository;
import com.bloggios.provider.utils.RandomGenerator;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
@Slf4j
public class PublishingIdGenerator {

    private static final String GENERATOR_DATA = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

    private final WebsitePublisherRepository websitePublisherRepository;

    public String generate() {
        log.info("{} >> generate -> Generating Publishing Id", getClass().getSimpleName());
        String sb;
        int limit = 0;
        do {
            sb =
                    "AP00" +
                            RandomGenerator.generateRandomString(GENERATOR_DATA, 4) +
                            RandomGenerator.generateRandomString(GENERATOR_DATA, 4);
            limit++;
            log.info("{} >> generate -> Limit: {}, publishingId: {}", getClass().getSimpleName(), limit, sb);
        } while (websitePublisherRepository.findByPublishingId(sb).isPresent() || limit < 6);
        return sb;
    }
}
