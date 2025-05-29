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
  File: WebsitePublisherServiceImplementation
 */

import com.ap.listing.dao.repository.WebsitePublisherRepository;
import com.ap.listing.enums.ErrorData;
import com.ap.listing.enums.WebsitePublishingStatus;
import com.ap.listing.exception.AuthenticationException;
import com.ap.listing.exception.BadRequestException;
import com.ap.listing.model.WebsitePublisher;
import com.ap.listing.payload.request.ManagePublisherRequest;
import com.ap.listing.payload.request.PublishWebsiteRequest;
import com.ap.listing.payload.response.ListResponse;
import com.ap.listing.payload.response.WebsitePublisherResponse;
import com.ap.listing.processor.PublishedWebsiteAnalyseApprovalProcessor;
import com.ap.listing.processor.UserIdAdditionInFilter;
import com.ap.listing.processor.WebsitePublisherToWebsiteSyncProcessor;
import com.ap.listing.processor.WebsitePublishingStatusAnalyser;
import com.ap.listing.properties.WebsitePublisherListProperties;
import com.ap.listing.service.WebsitePublisherService;
import com.ap.listing.transformer.PublishWebsiteRequestToWebsitePublisherTransformer;
import com.ap.listing.transformer.WebsitePublisherToResponseTransformer;
import com.ap.listing.utils.SecurityContextUtil;
import com.ap.listing.validator.NoUserIdInFilterValidator;
import com.ap.listing.validator.PublishWebsiteRequestValidator;
import com.bloggios.provider.payload.ModuleResponse;
import com.bloggios.provider.utils.ValueCheckerUtil;
import com.bloggios.query.payload.ListPayload;
import com.bloggios.query.processor.ListProcessor;
import com.bloggios.query.query.InitQuery;
import jakarta.persistence.TypedQuery;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Slf4j
public class WebsitePublisherServiceImplementation implements WebsitePublisherService {

    private final PublishWebsiteRequestValidator publishWebsiteRequestValidator;
    private final PublishWebsiteRequestToWebsitePublisherTransformer publishWebsiteRequestToWebsitePublisherTransformer;
    private final WebsitePublisherRepository websitePublisherRepository;
    private final WebsitePublisherToResponseTransformer websitePublisherToResponseTransformer;
    private final ListProcessor listProcessor;
    private final WebsitePublisherListProperties websitePublisherListProperties;
    private final InitQuery<WebsitePublisher> initQuery;
    private final NoUserIdInFilterValidator noUserIdInFilterValidator;
    private final UserIdAdditionInFilter userIdAdditionInFilter;
    private final WebsitePublishingStatusAnalyser websitePublishingStatusAnalyser;
    private final WebsitePublisherToWebsiteSyncProcessor websitePublisherToWebsiteSyncProcessor;
    private final PublishedWebsiteAnalyseApprovalProcessor publishedWebsiteAnalyseApprovalProcessor;

    @Override
    public ResponseEntity<ModuleResponse> publishSite(PublishWebsiteRequest publishWebsiteRequest, String websitePublisherId) {
        WebsitePublisher websitePublisher = websitePublisherRepository.findByPublishingId(websitePublisherId)
                .orElseThrow(() -> new BadRequestException(ErrorData.WEBSITE_PUBLISHER_NOT_FOUND, "websitePublisherId"));
        publishWebsiteRequestValidator.validate(publishWebsiteRequest);
        WebsitePublisher transformedWebsitePublisher = publishWebsiteRequestToWebsitePublisherTransformer.transform(publishWebsiteRequest, websitePublisher);
        WebsitePublisher analysed = websitePublishingStatusAnalyser.analyse(transformedWebsitePublisher);
        WebsitePublisher websitePublisherResponse = websitePublisherRepository.saveAndFlush(analysed);
        log.info("Website publisher saved to database: {}", websitePublisherResponse);
        CompletableFuture.runAsync(() -> publishedWebsiteAnalyseApprovalProcessor.process(websitePublisher.getPublishingId()));
        websitePublisherToWebsiteSyncProcessor.doSync(websitePublisherResponse);
        return ResponseEntity.ok(ModuleResponse
                .builder()
                .message("Website Published and currently in moderation process")
                .userId(UUID.fromString(SecurityContextUtil.getLoggedInUserOrThrow().getUserId()))
                .id(UUID.fromString(websitePublisherResponse.getWebsitePublisherId()))
                .build());
    }

    @Override
    public ResponseEntity<WebsitePublisherResponse> getPublishWebsite(String publishingId) {
        WebsitePublisher websitePublisher = websitePublisherRepository.findByPublishingId(publishingId)
                .orElseThrow(() -> new BadRequestException(ErrorData.WEBSITE_PUBLISHER_NOT_FOUND_PUBLISHING_ID));
        if (!websitePublisher.getUserId().equals(SecurityContextUtil.getLoggedInUserOrThrow().getUserId())) {
            throw new AuthenticationException(ErrorData.ACCESS_DENIED_GET_PUBLISH_WEBSITE, HttpStatus.FORBIDDEN);
        }
        WebsitePublisherResponse transform = websitePublisherToResponseTransformer.transform(websitePublisher);
        return ResponseEntity.ok(transform);
    }

    @Override
    public ResponseEntity<ListResponse> myPublishedWebsites(ListPayload listPayload) {
        noUserIdInFilterValidator.validate(listPayload);
        ListPayload processedListPayload = userIdAdditionInFilter.process(listPayload);
        ListPayload transformedListPayload = listProcessor.initProcess(processedListPayload, websitePublisherListProperties.getData(), "dateUpdated");
        TypedQuery<WebsitePublisher> build = initQuery.build(transformedListPayload, WebsitePublisher.class);
        List<WebsitePublisherResponse> websitePublisherResponses = build
                .getResultList()
                .stream()
                .map(websitePublisherToResponseTransformer::transform)
                .toList();
        ListResponse listResponse = ListResponse
                .builder()
                .object(websitePublisherResponses)
                .page(listPayload.getPage())
                .size(listPayload.getSize())
                .totalRecordsCount(initQuery.getTotalRecords(transformedListPayload, WebsitePublisher.class))
                .build();
        return ResponseEntity.ok(listResponse);
    }

    @Override
    public ResponseEntity<ListResponse> list(ListPayload listPayload) {
        ListPayload transformedListPayload = listProcessor.initProcess(listPayload, websitePublisherListProperties.getData(), "dateUpdated");
        TypedQuery<WebsitePublisher> build = initQuery.build(transformedListPayload, WebsitePublisher.class);
        List<WebsitePublisherResponse> websitePublisherResponses = build
                .getResultList()
                .stream()
                .map(websitePublisherToResponseTransformer::transform)
                .toList();
        ListResponse listResponse = ListResponse
                .builder()
                .object(websitePublisherResponses)
                .page(listPayload.getPage())
                .size(listPayload.getSize())
                .totalRecordsCount(initQuery.getTotalRecords(transformedListPayload, WebsitePublisher.class))
                .build();
        return ResponseEntity.ok(listResponse);
    }

    @Override
    public ResponseEntity<ModuleResponse> managePublisher(ManagePublisherRequest managePublisherRequest) {
        ValueCheckerUtil.isValidUUID(managePublisherRequest.getWebsitePublisherId(),
                () -> new BadRequestException(ErrorData.INVALID_WEBSITE_PUBLISHER_ID));
        WebsitePublisher websitePublisher = websitePublisherRepository.findById(managePublisherRequest.getWebsitePublisherId())
                .orElseThrow(() -> new BadRequestException(ErrorData.WEBSITE_PUBLISHER_NOT_FOUND));
        if (!websitePublisher.getWebsitePublishingStatus().equalsIgnoreCase(WebsitePublishingStatus.PENDING_MODERATION.name())) {
            throw new BadRequestException(ErrorData.WEBSITE_PUBLISHING_STATUS_NOT_MODERATION);
        }
        if (managePublisherRequest.getStatus().equals(WebsitePublishingStatus.DELETE)) {
            throw new BadRequestException(ErrorData.WEBSITE_PUBLISHER_CANNOT_BE_DELETED);
        }
        websitePublisher.setWebsitePublishingStatus(managePublisherRequest.getStatus().name());
        websitePublisher.setDateUpdated(new Date());
        websitePublisher.setMessage(managePublisherRequest.getMessage() + " -> moderatedBy : []" + SecurityContextUtil.getLoggedInUserOrThrow().getEmail());
        websitePublisher.setPublishingManager(SecurityContextUtil.getLoggedInUserOrThrow().getUserId());
        WebsitePublisher websitePublisherResponse = websitePublisherRepository.save(websitePublisher);
        log.info("Website publisher saved to database: {}", websitePublisherResponse.toString());
        return ResponseEntity.ok(ModuleResponse
                .builder()
                .message(String.format("Website Publisher %d", managePublisherRequest.getStatus().name()))
                .userId(UUID.fromString(SecurityContextUtil.getLoggedInUserOrThrow().getUserId()))
                .id(UUID.fromString(websitePublisherResponse.getWebsitePublisherId()))
                .build());
    }
}
