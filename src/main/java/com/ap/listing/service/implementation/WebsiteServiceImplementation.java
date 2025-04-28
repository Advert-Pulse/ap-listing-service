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
 * written permission from Advert Pulse or Bloggios.
 * 2. Reverse engineer, decompile, disassemble, or otherwise attempt to derive
 * the source code of the software.
 * 3. Modify this license in any way, including but not limited to altering its
 * terms, even by Advert Pulse or any other entity, without express written
 * permission from Bloggios administrators. Bloggios is the creator of this
 * license and retains exclusive rights to update or modify it.
 * 4. Update or modify the license without written permission from Bloggios
 * administrators.
 * <p>
 * The software is provided "as is," and Advert Pulse makes no warranties,
 * express or implied, regarding the software, including but not limited to any
 * warranties of merchantability, fitness for a particular purpose, or
 * non-infringement.
 * <p>
 * For inquiries regarding licensing, please contact support@bloggios.com.
 */
package com.ap.listing.service.implementation;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: WebsiteServiceImplementation
 */

import com.ap.listing.constants.ServiceConstants;
import com.ap.listing.dao.repository.DomainMetricsRepository;
import com.ap.listing.dao.repository.WebsiteRepository;
import com.ap.listing.enums.ErrorData;
import com.ap.listing.exception.BadRequestException;
import com.ap.listing.exception.BaseException;
import com.ap.listing.feign.DomainMetricsFeignClient;
import com.ap.listing.generator.AddWebsiteResponseGenerator;
import com.ap.listing.model.DomainMetrics;
import com.ap.listing.model.Website;
import com.ap.listing.model.WebsitePublisher;
import com.ap.listing.payload.response.AddWebsiteResponse;
import com.ap.listing.payload.response.DomainMetricsFeignResponse;
import com.ap.listing.processor.UrlAvailabilityWebsiteProcessor;
import com.ap.listing.processor.WebsiteDefaultPublisherProcessor;
import com.ap.listing.service.WebsiteService;
import com.ap.listing.transformer.DomainMetricsFeignResponseToDomainMetricsTransformer;
import com.ap.listing.transformer.WebsiteTransformer;
import com.ap.listing.utils.ExtractBaseUrl;
import com.ap.listing.utils.UrlChecker;
import com.bloggios.provider.payload.ModuleResponse;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import static com.ap.listing.constants.ServiceConstants.HTTPS;

@Service
@RequiredArgsConstructor
@Slf4j
public class WebsiteServiceImplementation implements WebsiteService {

    private final DomainMetricsFeignClient domainMetricsFeignClient;
    private final WebsiteTransformer websiteTransformer;
    private final WebsiteRepository websiteRepository;
    private final DomainMetricsFeignResponseToDomainMetricsTransformer domainMetricsFeignResponseToDomainMetricsTransformer;
    private final DomainMetricsRepository domainMetricsRepository;
    private final WebsiteDefaultPublisherProcessor websiteDefaultPublisherProcessor;
    private final UrlAvailabilityWebsiteProcessor urlAvailabilityWebsiteProcessor;

    @Override
    @Transactional
    public ResponseEntity<AddWebsiteResponse> addWebsite(String website) {
        String domain = website.toLowerCase();
        if (!domain.startsWith(ServiceConstants.HTTP) && !domain.startsWith(HTTPS)) {
            domain = HTTPS + domain;
        }
        String baseUrl = ExtractBaseUrl.extractBaseUrl(domain);
        Optional<Website> byDomain = websiteRepository.findByDomain(domain);
        boolean urlAvailable = UrlChecker.isUrlAvailable(baseUrl);
        if (byDomain.isPresent()) {
            urlAvailabilityWebsiteProcessor.process(byDomain.get(), urlAvailable);
            WebsitePublisher websitePublisher = websiteDefaultPublisherProcessor.process(byDomain.get());
            return AddWebsiteResponseGenerator.generate(byDomain.get(), websitePublisher, Boolean.FALSE);
        } else {
            if (!urlAvailable) {
                throw new BadRequestException(ErrorData.WEBSITE_IRRESPONSIVE, "website");
            }
            String feignUrl;
            if (baseUrl.startsWith(HTTPS)) {
                feignUrl = baseUrl.substring(8);
            } else {
                feignUrl = baseUrl.substring(7);
            }
            DomainMetricsFeignResponse domainMetrics = domainMetricsFeignClient.getDomainMetrics(feignUrl);
            log.info("Domain Metrics Response: {}", domainMetrics);
            if (domainMetrics.getDomain() == null) {
                throw new BadRequestException(ErrorData.WEBSITE_IRRESPONSIVE, "website");
            }
            Website websiteEntity = websiteTransformer.transform(baseUrl);
            Website websiteResponse = websiteRepository.save(websiteEntity);
            log.info("Website Saved : {}", websiteResponse);
            DomainMetrics domainMetricsTransform = domainMetricsFeignResponseToDomainMetricsTransformer.transform(domainMetrics, websiteResponse);
            DomainMetrics domainMetricsResponse = domainMetricsRepository.save(domainMetricsTransform);
            log.info("Domain Metrics Saved : {}", domainMetricsResponse);
            WebsitePublisher websitePublisher = websiteDefaultPublisherProcessor.process(websiteEntity);
            return AddWebsiteResponseGenerator.generate(websiteResponse, websitePublisher, Boolean.FALSE);
        }
    }

    @Override
    public ResponseEntity<List<AddWebsiteResponse>> addMultipleWebsite(List<String> websites) {
        if (CollectionUtils.isEmpty(websites)) {
            throw new BadRequestException(ErrorData.WEBSITE_IRRESPONSIVE, "websites");
        }
        if (websites.size() == 1) {
            throw new BadRequestException(ErrorData.WEBSITE_IRRESPONSIVE, "websites");
        }
        if (websites.size() > 5) {
            throw new BadRequestException(ErrorData.WEBSITE_IRRESPONSIVE, "websites");
        }
        List<AddWebsiteResponse> list = new ArrayList<>();
        websites
                .stream()
                .filter(Objects::nonNull)
                .forEach(website -> {
                    try {
                        ResponseEntity<AddWebsiteResponse> addWebsiteResponseResponseEntity = this.addWebsite(website);
                        AddWebsiteResponse body = addWebsiteResponseResponseEntity.getBody();
                        list.add(body);
                    } catch (BaseException e) {
                        log.error("Failed to add website: {}", website);
                        log.error("Error: {}", e.getMessage());
                    }
                });
        return ResponseEntity.ok(list);
    }
}