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
package com.ap.listing.service.implementation;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: WebsiteServiceImplementation
 */

import com.ap.listing.constants.ServiceConstants;
import com.ap.listing.dao.repository.WebsiteRepository;
import com.ap.listing.enums.ErrorData;
import com.ap.listing.exception.BadRequestException;
import com.ap.listing.exception.BaseException;
import com.ap.listing.feign.DomainMetricsFeignClient;
import com.ap.listing.generator.AddWebsiteResponseGenerator;
import com.ap.listing.model.WebsiteData;
import com.ap.listing.model.WebsitePublisher;
import com.ap.listing.payload.response.AddWebsiteResponse;
import com.ap.listing.payload.response.ListResponse;
import com.ap.listing.payload.response.WebsiteResponse;
import com.ap.listing.processor.UrlAvailabilityWebsiteProcessor;
import com.ap.listing.processor.WebsiteDefaultPublisherProcessor;
import com.ap.listing.properties.WebsiteListProperties;
import com.ap.listing.scheduler.generator.WebsiteDataFetchSchedulerGenerator;
import com.ap.listing.service.WebsiteService;
import com.ap.listing.transformer.WebsiteToWebsiteResponseTransformer;
import com.ap.listing.transformer.WebsiteTransformer;
import com.ap.listing.utils.ExtractBaseUrl;
import com.ap.listing.utils.UrlChecker;
import com.bloggios.query.payload.ListPayload;
import com.bloggios.query.processor.ListProcessor;
import com.bloggios.query.query.InitQuery;
import jakarta.persistence.TypedQuery;
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
    private final WebsiteDefaultPublisherProcessor websiteDefaultPublisherProcessor;
    private final UrlAvailabilityWebsiteProcessor urlAvailabilityWebsiteProcessor;
    private final ListProcessor listProcessor;
    private final WebsiteListProperties websiteListProperties;
    private final InitQuery<WebsiteData> initQuery;
    private final WebsiteToWebsiteResponseTransformer websiteToWebsiteResponseTransformer;
    private final WebsiteDataFetchSchedulerGenerator websiteDataFetchSchedulerGenerator;

    @Override
    public ResponseEntity<AddWebsiteResponse> addWebsite(String website) {
        String domain = website.toLowerCase();
        if (!domain.startsWith(ServiceConstants.HTTP) && !domain.startsWith(HTTPS)) {
            domain = HTTPS + domain;
        }
        String baseUrl = ExtractBaseUrl.extractBaseUrl(domain);
        Optional<WebsiteData> byDomain = websiteRepository.findByDomain(domain);
        boolean urlAvailable = UrlChecker.isUrlAvailable(baseUrl);
        if (byDomain.isPresent()) {
            urlAvailabilityWebsiteProcessor.process(byDomain.get(), urlAvailable);
            WebsitePublisher websitePublisher = websiteDefaultPublisherProcessor.process(byDomain.get());
            return AddWebsiteResponseGenerator.generate(byDomain.get(), websitePublisher, Boolean.FALSE);
        } else {
            if (!urlAvailable) {
                throw new BadRequestException(ErrorData.WEBSITE_IRRESPONSIVE, "website");
            }
            WebsiteData websiteDataEntity = websiteTransformer.transform(baseUrl);
            WebsiteData websiteDataResponse = websiteRepository.save(websiteDataEntity);
            log.info("Website Saved : {}", websiteDataResponse);
            WebsitePublisher websitePublisher = websiteDefaultPublisherProcessor.process(websiteDataEntity);
            websiteDataFetchSchedulerGenerator.process(websiteDataResponse.getWebsiteId());
            return AddWebsiteResponseGenerator.generate(websiteDataResponse, websitePublisher, Boolean.FALSE);
        }
    }

    @Override
    public ResponseEntity<List<AddWebsiteResponse>> addMultipleWebsite(List<String> websites) {
        if (CollectionUtils.isEmpty(websites)) {
            throw new BadRequestException(ErrorData.NO_LIST_ADDED, "No list of websites");
        }
        if (websites.size() == 1) {
            throw new BadRequestException(ErrorData.ONLY_ONE_WEBSITE_ADDED, "Only one website added");
        }
        if (websites.size() > 5) {
            throw new BadRequestException(ErrorData.WEBSITE_MAX_SIZE, "Maximum number of websites allowed is 5");
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

    @Override
    public ResponseEntity<ListResponse> list(ListPayload listPayload) {
        ListPayload transformedListPayload = listProcessor.initProcess(listPayload, websiteListProperties.getData(), "dateUpdated");
        TypedQuery<WebsiteData> build = initQuery.build(transformedListPayload, WebsiteData.class);
        List<WebsiteResponse> list = build
                .getResultList()
                .stream()
                .map(websiteToWebsiteResponseTransformer::transform)
                .toList();
        ListResponse listResponse = ListResponse
                .builder()
                .object(list)
                .page(listPayload.getPage())
                .size(listPayload.getSize())
                .totalRecordsCount(initQuery.getTotalRecords(transformedListPayload, WebsiteData.class))
                .build();
        return ResponseEntity.ok(listResponse);
    }
}