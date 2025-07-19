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
package com.ap.listing.scheduler.processor;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: FetchWebsiteDataScheduler
 */

import com.ap.listing.dao.repository.SchedulerRepository;
import com.ap.listing.dao.repository.WebsiteRepository;
import com.ap.listing.feign.AhrefFeignClient;
import com.ap.listing.feign.DomainMetricsFeignClient;
import com.ap.listing.feign.SimilarWebFeignClient;
import com.ap.listing.model.Scheduler;
import com.ap.listing.model.WebsiteData;
import com.ap.listing.payload.*;
import com.ap.listing.payload.response.AhrefWebsiteTrafficResponse;
import com.ap.listing.payload.response.DomainMetricsFeignResponse;
import com.ap.listing.utils.CountryNameUtil;
import com.ap.listing.utils.IntegerUtils;
import com.ap.listing.utils.MessageUtil;
import com.bloggios.provider.utils.DateUtils;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;

import java.util.*;
import java.util.concurrent.CompletableFuture;

import static com.ap.listing.constants.ServiceConstants.HTTPS;

@Component
@RequiredArgsConstructor
@Slf4j
public class FetchWebsiteDataSchedulerProcessor {

    private final WebsiteRepository websiteRepository;
    private final DomainMetricsFeignClient domainMetricsFeignClient;
    private final SchedulerRepository schedulerRepository;
    private final AhrefFeignClient ahrefFeignClient;
    private final SimilarWebFeignClient similarWebFeignClient;

    @Value("${scheduler.fetchWebsite.limit}")
    private int limit;

    @Value("${scheduler.fetchWebsite.multiplier}")
    private int multiplier;

    @Value("${website-analyser.mozDA}")
    private int mozDALimit;

    @Value("${website-analyser.domainRating}")
    private int domainRatingLimit;

    @Transactional
    @SneakyThrows(
            value = {
                    JsonProcessingException.class
            }
    )
    public void process(Scheduler scheduler) {
        log.info("{} >> {}", getClass().getSimpleName(), scheduler.toString());
        try {
            String websiteId = scheduler.getPrimaryId();
            Date now = new Date();
            Optional<WebsiteData> websiteOptional = websiteRepository.findById(websiteId);
            if (websiteOptional.isEmpty()) {
                scheduler.setIsSchedulingDone(true);
                scheduler.setScheduleCompletedOn(now);
                scheduler.setUpdatedOn(now);
                scheduler.setIsPassed(false);
                scheduler.setMessage(MessageUtil.getMessage(scheduler.getMessage(), "Website is not found with the given primary id: " + websiteId));
                Scheduler schedulerResponse = schedulerRepository.saveAndFlush(scheduler);
                log.info("Scheduler saved successfully to database {}", schedulerResponse.toString());
                return;
            }
            WebsiteData websiteData = websiteOptional.get();
            String baseUrl = websiteData.getDomain();
            String feignUrl;
            if (baseUrl.startsWith(HTTPS)) {
                feignUrl = baseUrl.substring(8);
            } else {
                feignUrl = baseUrl.substring(7);
            }
            CompletableFuture<ResponseEntity<DomainMetricsFeignResponse>> domainMetricsResponseFuture = CompletableFuture.supplyAsync(() -> domainMetricsFeignClient.getDomainMetrics(feignUrl));
            CompletableFuture<ResponseEntity<String>> ahrefBacklinkResponseFuture = CompletableFuture.supplyAsync(() -> ahrefFeignClient.getBacklinkResponse(feignUrl));
            CompletableFuture<ResponseEntity<String>> ahrefWebsiteTrafficResponseFuture = CompletableFuture.supplyAsync(() -> ahrefFeignClient.getWebsiteTraffic(feignUrl));
            CompletableFuture<ResponseEntity<SimilarWebTrafficHistoryWrapper>> similarWebWebsiteTrafficResponseFuture = CompletableFuture.supplyAsync(() -> similarWebFeignClient.getWebsiteTraffic(feignUrl));
            CompletableFuture.allOf(
                    domainMetricsResponseFuture,
                    ahrefBacklinkResponseFuture,
                    ahrefWebsiteTrafficResponseFuture,
                    similarWebWebsiteTrafficResponseFuture
            );
            ResponseEntity<DomainMetricsFeignResponse> domainMetricsResponse = domainMetricsResponseFuture.join();
            ResponseEntity<String> ahrefBacklinkResponse = ahrefBacklinkResponseFuture.join();
            ResponseEntity<String> ahrefWebsiteTrafficResponse = ahrefWebsiteTrafficResponseFuture.join();
            ResponseEntity<SimilarWebTrafficHistoryWrapper> similarWebWebsiteTrafficResponse = similarWebWebsiteTrafficResponseFuture.join();
            if (domainMetricsResponse.getStatusCode().is2xxSuccessful() && ahrefBacklinkResponse.getStatusCode().is2xxSuccessful() && ahrefWebsiteTrafficResponse.getStatusCode().is2xxSuccessful() && similarWebWebsiteTrafficResponse.getStatusCode().is2xxSuccessful()) {
                ObjectMapper objectMapper = new ObjectMapper();
                DomainMetricsFeignResponse domainMetrics = domainMetricsResponse.getBody();
                String ahrefBacklinkString = ahrefBacklinkResponse.getBody();
                String ahrefWebsiteTrafficString = ahrefWebsiteTrafficResponse.getBody();
                AhrefWebsiteAuthorityCheckerResponse ahrefBacklink = objectMapper.readValue(ahrefBacklinkString, AhrefWebsiteAuthorityCheckerResponse.class);
                AhrefWebsiteTrafficResponse ahrefWebsiteTraffic = objectMapper.readValue(ahrefWebsiteTrafficString, AhrefWebsiteTrafficResponse.class);
                SimilarWebTrafficHistoryWrapper similarWebWebsiteTraffic = similarWebWebsiteTrafficResponse.getBody();
                boolean analyseDomainMetrics = analyseDomainMetrics(domainMetrics);
                boolean analyseDomainRating = analyseDomainRating(ahrefBacklink);
                websiteData.setMozDa(IntegerUtils.getOrZero(domainMetrics.getMozDA()));
                websiteData.setMajesticTf(IntegerUtils.getOrZero(domainMetrics.getMajesticTF()));
                websiteData.setDomainRating(ahrefBacklink.getOverview().getDomainRating());
                if (!analyseDomainMetrics) {
                    websiteData.setIsActive(Boolean.FALSE.toString());
                    websiteData.setMessage("MozDA is less than 10");
                }
                if (!analyseDomainRating) {
                    String message = websiteData.getMessage() != null ? websiteData.getMessage() : "";
                    websiteData.setIsActive(Boolean.FALSE.toString());
                    websiteData.setMessage("Domain Rating is less than 10 " + message);
                }
                if (analyseDomainMetrics && analyseDomainRating) {
                    websiteData.setIsActive(Boolean.TRUE.toString());
                }
                websiteData.setAhrefOrganicTraffic(ahrefWebsiteTraffic.getTraffic().getTrafficMonthlyAvg());
                websiteData.setSimilarWebTraffic(similarWebWebsiteTraffic.getDomainAnalytics().getEngagements().getVisits());
                websiteData.setUrlRating(ahrefBacklink.getOverview().getUrlRating());
                websiteData.setCountryCode(similarWebWebsiteTraffic.getDomainAnalytics().getCountryRank().getCountryCode());
                websiteData.setCountry(CountryNameUtil.getCountryName(similarWebWebsiteTraffic.getDomainAnalytics().getCountryRank().getCountryCode()));
                websiteData.setAhrefTrafficHistory(ahrefWebsiteTraffic.getTrafficHistory());
                websiteData.setAhrefTopCountries(ahrefWebsiteTraffic.getTopCountries());
                websiteData.setSimilarWebTrafficHistory(processSimilarWebTraffic(similarWebWebsiteTraffic));
                websiteData.setSimilarWebTopCountries(processTopCountry(similarWebWebsiteTraffic));
                WebsiteData websiteDataResponse = websiteRepository.save(websiteData);
                log.info("Website Data Saved Successfully {} ", websiteDataResponse.toString());
                scheduler.setScheduleCompletedOn(now);
                scheduler.setIsSchedulingDone(Boolean.TRUE);
                scheduler.setUpdatedOn(now);
                scheduler.setIsPassed(Boolean.TRUE);
                Scheduler schedulerResponse = schedulerRepository.save(scheduler);
                log.info("Scheduler Saved Successfully {} ", schedulerResponse.toString());
            } else {
                scheduler.setMessage(
                        MessageUtil.getMessage(
                                scheduler.getMessage(),
                                String.format("Feign Response Error for schedulerId: %s domainMetricsResponse: %s, ahrefBacklinkResponse: %s, ahrefWebsiteTrafficResponse: %s and similarWebWebsiteTrafficResponse: %s",
                                        scheduler.getSchedulerId(),
                                        domainMetricsResponse.getStatusCode(),
                                        ahrefBacklinkResponse.getStatusCode(),
                                        ahrefWebsiteTrafficResponse.getStatusCode(),
                                        similarWebWebsiteTrafficResponse.getStatusCode()
                                )
                        )
                );
                log.error(
                        "DomainMetricsFeignResponse: {}, AhrefBacklinkResponse: {}, AhrefTrafficResponse: {}, SimilarWebTrafficResponse: {}",
                        domainMetricsResponse.getBody(),
                        ahrefBacklinkResponse.getBody(),
                        ahrefWebsiteTrafficResponse.getBody(),
                        similarWebWebsiteTrafficResponse.getBody()
                );
                if (scheduler.getTimesUsed() <= limit) {
                    log.info("Scheduler Put on retry mode for Website Fetch Data Feign Response");
                    int timesUsed = scheduler.getTimesUsed();
                    scheduler.setTimesUsed(timesUsed + 1);
                    scheduler.setScheduledOn(DateUtils.addHours(now, timesUsed * multiplier));
                    scheduler.setUpdatedOn(now);
                    Scheduler schedulerResponse = schedulerRepository.save(scheduler);
                    log.info("Scheduler saved successfully to database {}", schedulerResponse.toString());
                } else {
                    scheduler.setIsSchedulingDone(true);
                    scheduler.setUpdatedOn(now);
                    scheduler.setIsPassed(Boolean.FALSE);
                    scheduler.setScheduleCompletedOn(now);
                    Scheduler schedulerResponse = schedulerRepository.save(scheduler);
                    log.info("Scheduler saved successfully to database {}", schedulerResponse.toString());
                }
            }
        } catch (RuntimeException exception) {
            Date now = new Date();
            scheduler.setMessage(
                    MessageUtil.getMessage(
                            scheduler.getMessage(),
                            exception.getLocalizedMessage()
                    )
            );
            log.error(
                    "Runtime Exception Occurred while processing the Fetch Website Data",
                    exception
            );
            if (scheduler.getTimesUsed() <= limit) {
                log.info("Scheduler Put on retry mode for Website Fetch Data Feign Response");
                int timesUsed = scheduler.getTimesUsed();
                scheduler.setTimesUsed(timesUsed + 1);
                scheduler.setScheduledOn(DateUtils.addHours(now, timesUsed * multiplier));
                scheduler.setUpdatedOn(now);
                Scheduler schedulerResponse = schedulerRepository.save(scheduler);
                log.info("Scheduler saved successfully to database {}", schedulerResponse.toString());
            } else {
                scheduler.setIsSchedulingDone(true);
                scheduler.setUpdatedOn(now);
                scheduler.setIsPassed(Boolean.FALSE);
                scheduler.setScheduleCompletedOn(now);
                Scheduler schedulerResponse = schedulerRepository.save(scheduler);
                log.info("Scheduler saved successfully to database {}", schedulerResponse.toString());
            }
        }
    }

    public boolean analyseDomainMetrics(DomainMetricsFeignResponse domainMetrics) {
        if (domainMetrics.getMozDA() == null) return false;
        try {
            int mozDA = Integer.parseInt(domainMetrics.getMozDA());
            return mozDA >= mozDALimit;
        } catch (Exception ignored) {
            log.info("Exception occurred while converting mozDA to int");
            return false;
        }
    }

    public boolean analyseDomainRating(AhrefWebsiteAuthorityCheckerResponse ahrefWebsiteAuthorityCheckerResponse) {
        if (ahrefWebsiteAuthorityCheckerResponse.getOverview() == null) return false;
        try {
            int domainRating = ahrefWebsiteAuthorityCheckerResponse.getOverview().getDomainRating();
            return domainRating > domainRatingLimit;
        } catch (Exception ignored) {
            log.info("Exception occurred while converting DR to int");
            return false;
        }
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
