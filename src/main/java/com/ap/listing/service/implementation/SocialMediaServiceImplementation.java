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
  File: SocialMediaServiceImplementation
 */

import com.ap.listing.dao.repository.SocialMediaRepository;
import com.ap.listing.enums.ErrorData;
import com.ap.listing.exception.BadRequestException;
import com.ap.listing.model.SocialMedia;
import com.ap.listing.payload.request.SocialMediaRequest;
import com.ap.listing.payload.response.ListResponse;
import com.ap.listing.payload.response.SocialMediaResponse;
import com.ap.listing.processor.UserIdAdditionInFilter;
import com.ap.listing.properties.SocialMediaListProperties;
import com.ap.listing.service.SocialMediaService;
import com.ap.listing.transformer.SocialMediaRequestToSocialMediaTransformer;
import com.ap.listing.transformer.SocialMediaToSocialMediaResponseTransformer;
import com.ap.listing.validator.NoUserIdInFilterValidator;
import com.bloggios.provider.utils.ValueCheckerUtil;
import com.bloggios.query.payload.ListPayload;
import com.bloggios.query.processor.ListProcessor;
import com.bloggios.query.query.InitQuery;
import jakarta.persistence.TypedQuery;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
@Slf4j
public class SocialMediaServiceImplementation implements SocialMediaService {

    private final SocialMediaRequestToSocialMediaTransformer socialMediaRequestToSocialMediaTransformer;
    private final SocialMediaRepository socialMediaRepository;
    private final SocialMediaToSocialMediaResponseTransformer socialMediaToSocialMediaResponseTransformer;
    private final NoUserIdInFilterValidator noUserIdInFilterValidator;
    private final UserIdAdditionInFilter userIdAdditionInFilter;
    private final InitQuery<SocialMedia> initQuery;
    private final ListProcessor listProcessor;
    private final SocialMediaListProperties socialMediaListProperties;

    @Override
    public ResponseEntity<SocialMediaResponse> addSocialMedia(SocialMediaRequest socialMediaRequest) {
        SocialMedia transformedSocialMedia = socialMediaRequestToSocialMediaTransformer.transform(socialMediaRequest);
        SocialMedia socialMedia = socialMediaRepository.save(transformedSocialMedia);
        log.info("Social Media saved to Database with response: {}", socialMedia);
        SocialMediaResponse socialMediaResponse = socialMediaToSocialMediaResponseTransformer.transform(socialMedia);
        return ResponseEntity.ok(socialMediaResponse);
    }

    @Override
    public ResponseEntity<ListResponse> myList(ListPayload listPayload) {
        noUserIdInFilterValidator.validate(listPayload);
        ListPayload processedListPayload = userIdAdditionInFilter.process(listPayload);
        ListPayload transformedListPayload = listProcessor.initProcess(processedListPayload, socialMediaListProperties.getData(), "dateUpdated");
        TypedQuery<SocialMedia> build = initQuery.build(transformedListPayload, SocialMedia.class);
        List<SocialMediaResponse> socialMediaResponses = build
                .getResultList()
                .stream()
                .map(socialMediaToSocialMediaResponseTransformer::transform)
                .toList();
        ListResponse listResponse = ListResponse
                .builder()
                .object(socialMediaResponses)
                .page(listPayload.getPage())
                .size(listPayload.getSize())
                .totalRecordsCount(initQuery.getTotalRecords(transformedListPayload, SocialMedia.class))
                .build();
        return ResponseEntity.ok(listResponse);
    }

    @Override
    public ResponseEntity<SocialMediaResponse> updateSocialMedia(SocialMediaRequest socialMediaRequest, String socialMediaId) {
        ValueCheckerUtil.isValidUUID(socialMediaId, ()-> new BadRequestException(ErrorData.SOCIAL_MEDIA_ID_INVALID));
        SocialMedia socialMedia = socialMediaRepository.findById(socialMediaId)
                .orElseThrow(() -> new BadRequestException(ErrorData.SOCIAL_MEDIA_NOT_FOUND));
        socialMedia.setUrl(socialMediaRequest.getUrl());
        socialMedia.setAssociatedSite(socialMediaRequest.getAssociatedSite());
        socialMedia.setPrice(socialMediaRequest.getPrice());
        socialMedia.setDateUpdated(new Date());
        SocialMedia socialMediaResponse = socialMediaRepository.save(socialMedia);
        log.info("Social Media saved to Database with response: {}", socialMediaResponse);
        return ResponseEntity.ok(socialMediaToSocialMediaResponseTransformer.transform(socialMedia));
    }
}
