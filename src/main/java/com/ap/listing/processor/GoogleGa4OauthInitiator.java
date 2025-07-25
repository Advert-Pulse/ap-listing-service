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
  File: GoogleGa4OauthInitiator
 */

import com.ap.listing.feign.GoogleAnalyticsAdminFeign;
import com.ap.listing.model.WebsitePublisher;
import com.ap.listing.payload.request.GoogleOauthGa4Request;
import com.ap.listing.payload.response.*;
import com.ap.listing.utils.HostNameExtractor;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.concurrent.CompletableFuture;

@Component
@RequiredArgsConstructor
@Slf4j
public class GoogleGa4OauthInitiator {

    private final GoogleAnalyticsAdminFeign googleAnalyticsAdminFeign;
    private final GA4DetailsProcessor gA4DetailsProcessor;

    public InitiateGA4OAuthResponse initiate(GoogleOauthGa4Request googleOauthGa4Request, WebsitePublisher websitePublisher) {
        String publisherHost = HostNameExtractor.extractHostName(websitePublisher.getDomain());
        InitiateGA4OAuthResponse response = new InitiateGA4OAuthResponse();
        response.setHostName(publisherHost);
        response.setDomain(websitePublisher.getDomain());
        GoogleAnalyticsAccountResponse googleAnalyticsAccountResponse = googleAnalyticsAdminFeign.getAccountDetails(
                String.format("Bearer %s", googleOauthGa4Request.getAccessToken())
        );
        log.info("Google Analytics Account Response : {}", googleAnalyticsAccountResponse.toJson());
        List<GoogleAnalyticsAccountDetails> accounts = googleAnalyticsAccountResponse.getAccounts();

        for (GoogleAnalyticsAccountDetails googleAnalyticsAccountDetails : accounts) {
            log.info("Fetching Property details for account: {}", googleAnalyticsAccountDetails.getName());
            GoogleAnalyticsPropertyResponse googleAnalyticsPropertyResponse = googleAnalyticsAdminFeign.getPropertyDetails(
                    String.format("Bearer %s", googleOauthGa4Request.getAccessToken()),
                    String.format("parent:%s", googleAnalyticsAccountDetails.getName())
            );
            log.info("Property details for account : {}", googleAnalyticsPropertyResponse.toJson());
            List<GoogleAnalyticsPropertyDetails> properties = googleAnalyticsPropertyResponse.getProperties();
            for (GoogleAnalyticsPropertyDetails googleAnalyticsPropertyDetails : properties) {
                log.info("Fetching Property details for property: {}", googleAnalyticsPropertyDetails.getName());
                String propertyId = googleAnalyticsPropertyDetails.getName().substring(11);
                GoogleAnalyticsDataStreamResponse dataStreams = googleAnalyticsAdminFeign.getDataStreams(
                        String.format("Bearer %s", googleOauthGa4Request.getAccessToken()),
                        propertyId
                );
                log.info("DataStream details for property : {}", dataStreams.toJson());
                List<GoogleAnalyticsDataStreamDetails> streams = dataStreams.getDataStreams();
                for (GoogleAnalyticsDataStreamDetails googleAnalyticsDataStreamDetails : streams) {
                    log.info("Processing Data Stream for property : {}", googleAnalyticsDataStreamDetails.getName());
                    String defaultUri = googleAnalyticsDataStreamDetails.getWebStreamData().getDefaultUri();
                    String googleAnalyticsHost = HostNameExtractor.extractHostName(defaultUri);
                    if (googleAnalyticsHost.equals(publisherHost)) {
                        log.info("Website Found the Google Analytics Host : {}", googleAnalyticsHost);
                        CompletableFuture.runAsync(
                                ()-> gA4DetailsProcessor.process(websitePublisher, propertyId, googleOauthGa4Request)
                        );
                        response.setExist(Boolean.TRUE);
                        response.setMessage(String.format("Processing the data from Google Analytics for Analytics Id : %s", googleAnalyticsDataStreamDetails.getWebStreamData().getMeasurementId()));
                        return response;
                    }
                }
            }
        }
        response.setExist(Boolean.FALSE);
        response.setMessage(String.format("No Data Streams found in Analytics for Host : %s. Please check whether the host %s is present in any of your account or not", publisherHost, publisherHost));
        return response;
    }
}
