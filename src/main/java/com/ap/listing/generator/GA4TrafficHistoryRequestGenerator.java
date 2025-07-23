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
  File: GA4TrafficHistoryRequestGenerator
 */

import com.ap.listing.payload.request.GA4RunReportRequest;
import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.List;

@UtilityClass
@Slf4j
public class GA4TrafficHistoryRequestGenerator {

    private static final DateTimeFormatter FORMATTER = DateTimeFormatter.ofPattern("yyyy-MM-dd");

    public GA4RunReportRequest buildRequestBody(String hostName) {
        LocalDate today = LocalDate.now();
        LocalDate endDate = today.minusMonths(1).withDayOfMonth(1);
        LocalDate startDate = endDate.minusMonths(5);

        String start = startDate.format(DateTimeFormatter.ISO_DATE);
        String end = endDate.format(DateTimeFormatter.ISO_DATE);

        GA4RunReportRequest ga4RunReportRequest = GA4RunReportRequest.builder()
                .dateRanges(List.of(new GA4RunReportRequest.DateRange(start, end)))
                .metrics(List.of(new GA4RunReportRequest.Metric("sessions")))
                .dimensions(List.of(
                        new GA4RunReportRequest.Dimension("yearMonth"),
                        new GA4RunReportRequest.Dimension("hostName")
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
                .orderBys(List.of(
                        new GA4RunReportRequest.OrderBy(
                                new GA4RunReportRequest.DimensionOrderBy("yearMonth")
                        )
                ))
                .build();

        log.info("GA4 Run Report request generated : {}", ga4RunReportRequest.toJson());
        return ga4RunReportRequest;
    }
}
