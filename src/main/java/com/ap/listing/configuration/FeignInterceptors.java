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
package com.ap.listing.configuration;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: RapidApiFeignConfig
 */

import feign.RequestInterceptor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
@Slf4j
public class FeignInterceptors {

    public static final String X_RAPIDAPI_HOST = "x-rapidapi-host";
    public static final String X_RAPIDAPI_KEY = "x-rapidapi-key";

    @Value("${feign-client.domain-metrics.host}")
    private String domainMetricsDomain;

    @Value("${feign-client.domain-metrics.api-key}")
    private String apiKey;

    @Value("${feign-client.ahref.website-traffic.host}")
    private String ahrefHost;

    @Value("${feign-client.similar-web.analytics.host}")
    private String similarWebHost;

    @Bean
    public RequestInterceptor requestInterceptor() {
        return requestTemplate -> {
            log.info("Feign Interceptor {}", requestTemplate);
            if (requestTemplate.url().contains("/traffic") || requestTemplate.url().contains("/backlinks")) {
                requestTemplate.header(X_RAPIDAPI_HOST, ahrefHost);
                requestTemplate.header(X_RAPIDAPI_KEY, apiKey);
            } else if (requestTemplate.url().contains("/analyticsv1")) {
                requestTemplate.header(X_RAPIDAPI_HOST, similarWebHost);
                requestTemplate.header(X_RAPIDAPI_KEY, apiKey);
            } else {
                requestTemplate.header(X_RAPIDAPI_HOST, domainMetricsDomain);
                requestTemplate.header(X_RAPIDAPI_KEY, apiKey);
            }
        };
    }
}
