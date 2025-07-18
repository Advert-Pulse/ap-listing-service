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
  File: OwnershipServiceImplementation
 */

import com.ap.listing.dao.repository.OwnershipDetailsRepository;
import com.ap.listing.dao.repository.WebsitePublisherRepository;
import com.ap.listing.dao.repository.WebsiteRepository;
import com.ap.listing.enums.ErrorData;
import com.ap.listing.exception.BadRequestException;
import com.ap.listing.model.OwnershipDetails;
import com.ap.listing.model.WebsiteData;
import com.ap.listing.model.WebsitePublisher;
import com.ap.listing.payload.response.OwnershipDetailsResponse;
import com.ap.listing.processor.CreateOrUpdateOwnershipProcessor;
import com.ap.listing.service.OwnershipService;
import com.ap.listing.transformer.OwnershipDetailsToResponseTransformer;
import com.ap.listing.transformer.OwnershipDetailsTransformer;
import com.bloggios.provider.payload.ModuleResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
@Slf4j
public class OwnershipServiceImplementation implements OwnershipService {

    private final OwnershipDetailsRepository ownershipDetailsRepository;
    private final CreateOrUpdateOwnershipProcessor createOrUpdateOwnershipProcessor;
    private final OwnershipDetailsToResponseTransformer ownershipDetailsToResponseTransformer;

    @Override
    public ResponseEntity<OwnershipDetailsResponse> createOrGetOwnershipDetails(String publishingId) {
        OwnershipDetailsResponse ownershipDetailsResponse = ownershipDetailsRepository.findByPublishingId(publishingId)
                .map(ownershipDetailsToResponseTransformer::transform)
                .orElseGet(() -> {
                    OwnershipDetails ownershipDetails = createOrUpdateOwnershipProcessor.createOwnershipDetails(publishingId);
                    return ownershipDetailsToResponseTransformer.transform(ownershipDetails);
                });
        return ResponseEntity.ok(ownershipDetailsResponse);
    }
}
