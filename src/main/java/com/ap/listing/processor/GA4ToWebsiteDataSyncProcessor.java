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
  File: GA4ToWebsiteDataSyncProcessor
 */

import com.ap.listing.dao.repository.WebsitePublisherRepository;
import com.ap.listing.dao.repository.WebsiteRepository;
import com.ap.listing.enums.ErrorData;
import com.ap.listing.exception.BadRequestException;
import com.ap.listing.model.GA4History;
import com.ap.listing.model.WebsiteData;
import com.ap.listing.model.WebsitePublisher;
import com.ap.listing.payload.GA4Channel;
import com.ap.listing.payload.TopCountry;
import com.ap.listing.payload.TrafficHistory;
import com.ap.listing.payload.records.GA4WebsiteDataRecord;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Objects;

@Component
@RequiredArgsConstructor
@Slf4j
public class GA4ToWebsiteDataSyncProcessor {

    private final GA4TrafficHistoryProcessor gA4TrafficHistoryProcessor;
    private final GA4TrafficProcessor gA4TrafficProcessor;
    private final GA4CountryTrafficHistoryProcessor gA4CountryTrafficHistoryProcessor;
    private final GA4ChannelProcessor gA4ChannelProcessor;
    private final WebsitePublisherRepository websitePublisherRepository;
    private final WebsiteRepository websiteRepository;

    public void process(GA4WebsiteDataRecord gA4WebsiteDataRecord, GA4History ga4History) {
        List<TrafficHistory> trafficHistoryList = gA4TrafficHistoryProcessor.process(gA4WebsiteDataRecord.trafficHistoryResponse(), ga4History);
        Long traffic = gA4TrafficProcessor.process(gA4WebsiteDataRecord.trafficResponse(), ga4History);
        List<TopCountry> countryTrafficHistory = gA4CountryTrafficHistoryProcessor.process(gA4WebsiteDataRecord.countryTrafficHistoryResponse(), ga4History);
        GA4Channel gA4Channel = gA4ChannelProcessor.process(gA4WebsiteDataRecord.channelResponse(), ga4History);
        String publishingId = ga4History.getPublishingId();
        WebsitePublisher websitePublisher = websitePublisherRepository.findByPublishingId(publishingId)
                .orElseThrow(() -> new BadRequestException(ErrorData.WEBSITE_PUBLISHER_NOT_FOUND_PUBLISHING_ID));
        String websiteId = websitePublisher.getWebsiteData().getWebsiteId();
        if (Objects.isNull(trafficHistoryList) || Objects.isNull(traffic) || Objects.isNull(countryTrafficHistory) || Objects.isNull(gA4Channel)) return;
        WebsiteData websiteData = websiteRepository.findById(websiteId)
                .orElseThrow(() -> new BadRequestException(ErrorData.WEBSITE_NOT_FOUND_BY_ID));
        websiteData.setGa4TrafficHistory(trafficHistoryList);
        websiteData.setGa4Channel(gA4Channel);
        websiteData.setGa4Traffic(traffic);
        websiteData.setGa4TopCountries(countryTrafficHistory);
        websiteData.setIsGa4Metrics(Boolean.TRUE.toString());
        WebsiteData websiteDataResponse = websiteRepository.save(websiteData);
        log.info("websiteDataResponse={}", websiteDataResponse.toString());
    }
}
