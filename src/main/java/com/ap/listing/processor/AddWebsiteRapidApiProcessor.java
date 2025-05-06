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
  File: AddWebsiteRapidApiProcessor
 */

import com.ap.listing.dao.repository.WebsiteRepository;
import com.ap.listing.feign.AhrefFeignClient;
import com.ap.listing.feign.SimilarWebFeignClient;
import com.ap.listing.model.Website;
import com.ap.listing.payload.*;
import com.ap.listing.payload.response.AhrefWebsiteTrafficResponse;
import com.ap.listing.payload.response.DomainMetricsFeignResponse;
import com.ap.listing.utils.CountryNameUtil;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

@Component
@RequiredArgsConstructor
@Slf4j
public class AddWebsiteRapidApiProcessor {

    private final AhrefFeignClient ahrefFeignClient;
    private final SimilarWebFeignClient similarWebFeignClient;
    private final WebsiteRepository websiteRepository;

    @Async
    public void process(String website, DomainMetricsFeignResponse domainMetricsFeignResponse, Website websiteEntity) {
        long startTime = System.currentTimeMillis();
        log.info("Add Website Rapid Api Processor: {}", website);
        AhrefWebsiteTrafficResponse ahrefWebsiteTraffic = ahrefFeignClient.getWebsiteTraffic(website);
        log.info("Ahref Website Traffic Response: {}", ahrefWebsiteTraffic);
        AhrefBacklinkResponse ahrefBacklinkResponse = ahrefFeignClient.getBacklinkResponse(website);
        log.info("Ahref Backlink Response: {}", ahrefBacklinkResponse);
        SimilarWebTrafficHistoryWrapper similarWebWebsiteTraffic = similarWebFeignClient.getWebsiteTraffic(website);
        log.info("Similar Web Website Traffic Response: {}", similarWebWebsiteTraffic);
        websiteEntity.setMozDa(domainMetricsFeignResponse.getMozDA());
        websiteEntity.setMajesticTf(Integer.parseInt(domainMetricsFeignResponse.getMajesticTF()));
        websiteEntity.setAhrefOrganicTraffic(ahrefWebsiteTraffic.getTrafficMonthlyAvg());
        websiteEntity.setSimilarWebTraffic(similarWebWebsiteTraffic.getDomainAnalytics().getEngagements().getVisits());
        websiteEntity.setDomainRating(Integer.parseInt(ahrefBacklinkResponse.getDomainRating()));
        websiteEntity.setUrlRating(Integer.parseInt(ahrefBacklinkResponse.getUrlRating()));
        websiteEntity.setCountryCode(similarWebWebsiteTraffic.getDomainAnalytics().getCountryRank().getCountryCode());
        websiteEntity.setCountry(CountryNameUtil.getCountryName(similarWebWebsiteTraffic.getDomainAnalytics().getCountryRank().getCountryCode()));
        websiteEntity.setAhrefTrafficHistory(ahrefWebsiteTraffic.getTrafficHistory());
        websiteEntity.setAhrefTopCountries(ahrefWebsiteTraffic.getTopCountries());
        websiteEntity.setSimilarWebTrafficHistory(processSimilarWebTraffic(similarWebWebsiteTraffic));
        websiteEntity.setSimilarWebTopCountries(processTopCountry(similarWebWebsiteTraffic));
        Website websiteEntityResponse = websiteRepository.save(websiteEntity);
        log.info("Website Entity Response: {}", websiteEntityResponse);
        log.info("Commiting changes for adding the website in {}ms", System.currentTimeMillis() - startTime);
    }

    private List<TrafficHistory> processSimilarWebTraffic(SimilarWebTrafficHistoryWrapper similarWebTrafficHistoryWrapper) {
        SimilarWebTrafficHistory domainAnalytics = similarWebTrafficHistoryWrapper.getDomainAnalytics();
        if (domainAnalytics == null) return Collections.emptyList();
        Map<String, Long> estimatedMonthlyVisits = domainAnalytics.getEstimatedMonthlyVisits();
        List<TrafficHistory> trafficHistories = new ArrayList<>();
        estimatedMonthlyVisits.forEach((s, aLong) -> trafficHistories.add(new TrafficHistory(s, aLong)));
        log.info("Processed Similar Web Traffic History: {}", trafficHistories);
        return trafficHistories;
    }

    private List<TopCountry> processTopCountry(SimilarWebTrafficHistoryWrapper similarWebTrafficHistoryWrapper) {
        SimilarWebTrafficHistory domainAnalytics = similarWebTrafficHistoryWrapper.getDomainAnalytics();
        if (domainAnalytics == null) return Collections.emptyList();
        List<TopCountry> list = domainAnalytics.getTopCountryShares()
                .stream()
                .map(similarWebCountryShares -> new TopCountry(similarWebCountryShares.getCountryCode(), Double.parseDouble(similarWebCountryShares.getValueinPercentage())))
                .toList();
        log.info("Processed Similar Web Top Country: {}", list);
        return list;
    }
}
