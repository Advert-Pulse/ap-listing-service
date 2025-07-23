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
  File: GA4DetailsProcessor
 */

import com.ap.listing.feign.GoogleAnalyticsDataFeign;
import com.ap.listing.generator.GA4ChannelGroupRequestGenerator;
import com.ap.listing.generator.GA4CountryTrafficHistoryGenerator;
import com.ap.listing.generator.GA4TrafficHistoryRequestGenerator;
import com.ap.listing.generator.GA4TrafficRequestGenerator;
import com.ap.listing.model.WebsitePublisher;
import com.ap.listing.payload.request.GA4RunReportRequest;
import com.ap.listing.payload.request.GoogleOauthGa4Request;
import com.ap.listing.payload.response.GA4RunReportResponse;
import com.ap.listing.utils.HostNameExtractor;
import com.google.analytics.data.v1beta.RunReportRequest;
import com.google.analytics.data.v1beta.RunReportResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
@Slf4j
public class GA4DetailsProcessor {

    private final GoogleAnalyticsDataFeign googleAnalyticsDataFeign;

    public void process(WebsitePublisher websitePublisher, String propertyId, GoogleOauthGa4Request googleOauthGa4Request) {
        String hostName = HostNameExtractor.extractHostName(websitePublisher.getDomain());
        GA4RunReportRequest trafficHistoryRequest = GA4TrafficHistoryRequestGenerator.buildRequestBody(hostName);
        GA4RunReportResponse trafficHistoryResponse = googleAnalyticsDataFeign.runReportRequest(
                String.format("Bearer %s", googleOauthGa4Request.getAccessToken()),
                trafficHistoryRequest,
                propertyId
        );

        GA4RunReportRequest trafficRequest = GA4TrafficRequestGenerator.buildRequestBody(hostName);
        GA4RunReportResponse trafficResponse = googleAnalyticsDataFeign.runReportRequest(
                String.format("Bearer %s", googleOauthGa4Request.getAccessToken()),
                trafficRequest,
                propertyId
        );

        GA4RunReportRequest channelRequest = GA4ChannelGroupRequestGenerator.buildRequestBody(hostName);
        GA4RunReportResponse channelResponse = googleAnalyticsDataFeign.runReportRequest(
                String.format("Bearer %s", googleOauthGa4Request.getAccessToken()),
                channelRequest,
                propertyId
        );

        GA4RunReportRequest countryHistoryRequest = GA4CountryTrafficHistoryGenerator.buildRequestBody(hostName);

        GA4RunReportResponse countryTrafficHistoryResponse = googleAnalyticsDataFeign.runReportRequest(
                String.format("Bearer %s", googleOauthGa4Request.getAccessToken()),
                countryHistoryRequest,
                propertyId
        );
    }
}
