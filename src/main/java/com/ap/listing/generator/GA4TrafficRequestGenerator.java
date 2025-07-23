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
package com.ap.listing.generator;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: GA4TrafficRequestGenerator
 */

import com.ap.listing.payload.request.GA4RunReportRequest;
import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;

import java.util.List;

@UtilityClass
@Slf4j
public class GA4TrafficRequestGenerator {

    public static GA4RunReportRequest buildRequestBody(String hostName) {
        return GA4RunReportRequest.builder()
                .dateRanges(List.of(
                        GA4RunReportRequest.DateRange.builder()
                                .startDate("30daysAgo")
                                .endDate("today")
                                .build()
                ))
                .metrics(List.of(
                        GA4RunReportRequest.Metric.builder().name("sessions").build()
                ))
                .dimensions(List.of(
                        GA4RunReportRequest.Dimension.builder().name("hostName").build()
                ))
                .dimensionFilter(GA4RunReportRequest.FilterExpression.builder()
                        .filter(GA4RunReportRequest.Filter.builder()
                                .fieldName("hostName")
                                .stringFilter(GA4RunReportRequest.StringFilter.builder()
                                        .value(hostName)
                                        .matchType("EXACT")
                                        .build())
                                .build())
                        .build())
                .build();
    }
}
