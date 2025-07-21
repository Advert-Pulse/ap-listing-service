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
  File: VerifyOwnershipProcessor
 */

import com.ap.listing.enums.ErrorData;
import com.ap.listing.exception.BadRequestException;
import com.ap.listing.utils.NonClientUrlChecker;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.HttpStatusCode;
import org.springframework.stereotype.Component;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Mono;

import java.time.Duration;

@Component
@RequiredArgsConstructor
@Slf4j
public class VerifyOwnershipProcessor {

    private final WebClient webClient;

    public String getLinkData(String link) {
        boolean urlAvailable = NonClientUrlChecker.isUrlAvailable(link);
        if (!urlAvailable)
            throw new BadRequestException(
                    ErrorData.WEBCLIENT_ERROR,
                    "link",
                    String.format("No static page found at %s. Please make sure that the downloaded file is present in the root of your website", link)
            );
        String content = webClient
                .get()
                .uri(link)
                .retrieve()
                .onStatus(
                        status -> status.isError(),
                        response -> {
                            return response.bodyToMono(String.class)
                                    .flatMap(body -> {
                                        return Mono.error(
                                                generateException(body, response.statusCode(), link)
                                        );
                                    });
                        }).bodyToMono(String.class)
                .timeout(Duration.ofSeconds(10))
                .block();
        log.info("Content received from webclient: {}", content);
        return content;
    }

    private Throwable generateException(String body, HttpStatusCode httpStatusCode, String link) {
        if (httpStatusCode.isSameCodeAs(HttpStatus.NOT_FOUND)) {
            return new BadRequestException(
                    ErrorData.WEBCLIENT_ERROR,
                    "link",
                    String.format("No static page found at %s. Please make sure that the downloaded file is present in the root of your website", link)
            );
        } else {
            return new BadRequestException(
                    ErrorData.WEBCLIENT_ERROR,
                    "link",
                    String.format("Unable to reach the verification link : %s", link)
            );
        }
    }
}
