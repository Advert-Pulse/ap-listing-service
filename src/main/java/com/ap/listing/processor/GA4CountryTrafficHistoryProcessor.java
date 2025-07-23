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
  File: GA4CountryTrafficHistoryProcessor
 */

import com.ap.listing.model.GA4History;
import com.ap.listing.payload.TopCountry;
import com.ap.listing.payload.response.GA4RunReportResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

@Component
@RequiredArgsConstructor
@Slf4j
public class GA4CountryTrafficHistoryProcessor {

    private final GA4HistoryPersistProcessor gA4HistoryPersistProcessor;

    public List<TopCountry> process(GA4RunReportResponse ga4RunReportResponse, GA4History ga4History) {
        if (Objects.isNull(ga4RunReportResponse.getRows()) || ga4RunReportResponse.getRows().isEmpty()) {
            log.error("Ga4History Rows is null or empty : {}", ga4RunReportResponse);
            gA4HistoryPersistProcessor.persist(ga4History, "No Rows present in the GA4CountryTrafficHistoryResponse");
            return null;
        }

        List<GA4RunReportResponse.Row> rows = ga4RunReportResponse.getRows();

        long totalSessions = 0;

        for (GA4RunReportResponse.Row row : rows) {
            List<GA4RunReportResponse.MetricValue> metrics = row.getMetricValues();
            if (metrics != null && !metrics.isEmpty()) {
                totalSessions += Long.parseLong(metrics.get(0).getValue());
            }
        }

        if (totalSessions == 0) {
            log.error("Ga4History Total Sessions is 0 : {}", ga4RunReportResponse);
            gA4HistoryPersistProcessor.persist(ga4History, "Ga4History Total Sessions is 0 in the GA4CountryTrafficHistoryResponse");
            return null;
        }

        List<TopCountry> topCountries = new ArrayList<>();
        for (GA4RunReportResponse.Row row : rows) {
            List<GA4RunReportResponse.DimensionValue> dimensions = row.getDimensionValues();
            List<GA4RunReportResponse.MetricValue> metrics = row.getMetricValues();

            if (dimensions.size() < 2 || metrics.isEmpty()) {
                continue;
            }

            String countryCode = dimensions.get(1).getValue(); // second dimension is country code
            long sessions = Long.parseLong(metrics.get(0).getValue());
            double share = (sessions * 100.0) / totalSessions;

            topCountries.add(new TopCountry(countryCode, share));
        }

        return topCountries;
    }
}
