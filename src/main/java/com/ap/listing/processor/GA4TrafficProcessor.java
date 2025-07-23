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
  File: GA4TrafficProcessor
 */

import com.ap.listing.model.GA4History;
import com.ap.listing.payload.response.GA4RunReportResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Objects;

@Component
@RequiredArgsConstructor
@Slf4j
public class GA4TrafficProcessor {

    private final GA4HistoryPersistProcessor gA4HistoryPersistProcessor;

    public Long process(GA4RunReportResponse ga4RunReportResponse, GA4History ga4History) {
        if (Objects.isNull(ga4RunReportResponse.getRows()) || ga4RunReportResponse.getRows().isEmpty()) {
            log.error("Ga4History Rows is null or empty : {}", ga4RunReportResponse);
            gA4HistoryPersistProcessor.persist(ga4History, "No Rows present in the GA4TrafficResponse");
            return null;
        }
        GA4RunReportResponse.Row row = ga4RunReportResponse.getRows().get(0);
        List<GA4RunReportResponse.MetricValue> metricValues = row.getMetricValues();

        if (metricValues == null || metricValues.isEmpty()) {
            log.error("Ga4History Metrics is null or empty : {}", ga4RunReportResponse);
            gA4HistoryPersistProcessor.persist(ga4History, "No Metrics present in the GA4TrafficResponse.MetricValue");
            return null;
        }

        try {
            return Long.parseLong(metricValues.get(0).getValue());
        } catch (NumberFormatException e) {
            log.error("Parsing Exception for long value : {}", metricValues.get(0).getValue());
            gA4HistoryPersistProcessor.persist(ga4History, "Parsing Exception for long value in the GA4TrafficResponse.MetricValue");
            return null;
        }
    }
}
