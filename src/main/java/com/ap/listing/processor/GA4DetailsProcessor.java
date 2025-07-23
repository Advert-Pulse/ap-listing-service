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
import com.ap.listing.model.GA4History;
import com.ap.listing.model.WebsitePublisher;
import com.ap.listing.payload.records.GA4WebsiteDataRecord;
import com.ap.listing.payload.request.GA4RunReportRequest;
import com.ap.listing.payload.request.GoogleOauthGa4Request;
import com.ap.listing.payload.response.GA4RunReportResponse;
import com.ap.listing.utils.AsyncUtils;
import com.ap.listing.utils.HostNameExtractor;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.Date;
import java.util.concurrent.CompletableFuture;

@Component
@RequiredArgsConstructor
@Slf4j
public class GA4DetailsProcessor {

    private final GoogleAnalyticsDataFeign googleAnalyticsDataFeign;
    private final GA4ToWebsiteDataSyncProcessor gA4ToWebsiteDataSyncProcessor;

    public void process(WebsitePublisher websitePublisher, String propertyId, GoogleOauthGa4Request googleOauthGa4Request) {
        String hostName = HostNameExtractor.extractHostName(websitePublisher.getDomain());
        CompletableFuture<GA4RunReportResponse> trafficHistoryFuture = CompletableFuture.supplyAsync(() ->
                this.processTrafficHistory(
                        googleOauthGa4Request,
                        hostName,
                        propertyId
                )
        );
        CompletableFuture<GA4RunReportResponse> trafficFuture = CompletableFuture.supplyAsync(() ->
                this.processTraffic(
                        googleOauthGa4Request,
                        hostName,
                        propertyId
                )
        );
        CompletableFuture<GA4RunReportResponse> channelFuture = CompletableFuture.supplyAsync(() ->
                this.processChannel(
                        googleOauthGa4Request,
                        hostName,
                        propertyId
                )
        );
        CompletableFuture<GA4RunReportResponse> countryTrafficHistoryFuture = CompletableFuture.supplyAsync(() ->
                this.processCountryTrafficHistory(
                        googleOauthGa4Request,
                        hostName,
                        propertyId
                )
        );
        AsyncUtils.getAsyncResult(CompletableFuture.allOf(trafficHistoryFuture, trafficFuture, channelFuture, countryTrafficHistoryFuture));
        GA4RunReportResponse trafficHistoryResponse = trafficHistoryFuture.join();
        GA4RunReportResponse trafficResponse = trafficFuture.join();
        GA4RunReportResponse channelResponse = channelFuture.join();
        GA4RunReportResponse countryTrafficHistoryResponse = countryTrafficHistoryFuture.join();
        GA4WebsiteDataRecord ga4WebsiteDataRecord = new GA4WebsiteDataRecord(
                trafficHistoryResponse,
                trafficResponse,
                channelResponse,
                countryTrafficHistoryResponse
        );
        GA4History ga4History = this.generate(ga4WebsiteDataRecord, websitePublisher.getPublishingId(), propertyId);
        gA4ToWebsiteDataSyncProcessor.process(
                ga4WebsiteDataRecord,
                ga4History
        );
    }

    private GA4RunReportResponse processTrafficHistory(
            GoogleOauthGa4Request googleOauthGa4Request,
            String hostName,
            String propertyId
    ) {
        GA4RunReportRequest trafficHistoryRequest = GA4TrafficHistoryRequestGenerator.buildRequestBody(hostName);
        return googleAnalyticsDataFeign.runReportRequest(
                String.format("Bearer %s", googleOauthGa4Request.getAccessToken()),
                trafficHistoryRequest,
                propertyId
        );
    }

    private GA4RunReportResponse processTraffic(
            GoogleOauthGa4Request googleOauthGa4Request,
            String hostName,
            String propertyId
    ) {
        GA4RunReportRequest trafficRequest = GA4TrafficRequestGenerator.buildRequestBody(hostName);
        return googleAnalyticsDataFeign.runReportRequest(
                String.format("Bearer %s", googleOauthGa4Request.getAccessToken()),
                trafficRequest,
                propertyId
        );
    }

    private GA4RunReportResponse processChannel(
            GoogleOauthGa4Request googleOauthGa4Request,
            String hostName,
            String propertyId
    ) {
        GA4RunReportRequest channelRequest = GA4ChannelGroupRequestGenerator.buildRequestBody(hostName);
        return googleAnalyticsDataFeign.runReportRequest(
                String.format("Bearer %s", googleOauthGa4Request.getAccessToken()),
                channelRequest,
                propertyId
        );
    }

    private GA4RunReportResponse processCountryTrafficHistory(
            GoogleOauthGa4Request googleOauthGa4Request,
            String hostName,
            String propertyId
    ) {
        GA4RunReportRequest countryHistoryRequest = GA4CountryTrafficHistoryGenerator.buildRequestBody(hostName);
        return googleAnalyticsDataFeign.runReportRequest(
                String.format("Bearer %s", googleOauthGa4Request.getAccessToken()),
                countryHistoryRequest,
                propertyId
        );
    }

    private GA4History generate(GA4WebsiteDataRecord ga4WebsiteDataRecord, String publishingId, String propertyId) {
        GA4History ga4History = GA4History
                .builder()
                .countryTrafficHistoryResponse(ga4WebsiteDataRecord.trafficHistoryResponse())
                .channelResponse(ga4WebsiteDataRecord.channelResponse())
                .trafficHistoryResponse(ga4WebsiteDataRecord.trafficHistoryResponse())
                .trafficResponse(ga4WebsiteDataRecord.trafficResponse())
                .dateCreated(new Date())
                .dateUpdated(new Date())
                .propertyId(propertyId)
                .publishingId(publishingId)
                .build();
        log.info("GA4History generated : {}", ga4History.toJson());
        return ga4History;
    }
}
