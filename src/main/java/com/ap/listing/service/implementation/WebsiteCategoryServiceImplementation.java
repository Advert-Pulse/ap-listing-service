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
  File: WebsiteCategoryServiceImplementation
 */

import com.ap.listing.dao.repository.WebsiteCategoryRepository;
import com.ap.listing.enums.ErrorData;
import com.ap.listing.exception.BadRequestException;
import com.ap.listing.model.WebsiteCategory;
import com.ap.listing.payload.request.WebsiteCategoryRequest;
import com.ap.listing.payload.response.ListResponse;
import com.ap.listing.payload.response.WebsiteCategoryResponse;
import com.ap.listing.service.WebsiteCategoryService;
import com.ap.listing.transformer.WebsiteCategoryRequestTransformer;
import com.ap.listing.transformer.WebsiteCategoryResponseTransformer;
import com.ap.listing.transformer.WebsiteCategoryResponsesToListResponseTransformer;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
@RequiredArgsConstructor
@Slf4j
public class WebsiteCategoryServiceImplementation implements WebsiteCategoryService {

    private final WebsiteCategoryRepository websiteCategoryRepository;
    private final WebsiteCategoryRequestTransformer websiteCategoryRequestTransformer;
    private final WebsiteCategoryResponseTransformer websiteCategoryResponseTransformer;
    private final WebsiteCategoryResponsesToListResponseTransformer websiteCategoryResponsesToListResponseTransformer;

    @Override
    public ResponseEntity<WebsiteCategoryResponse> addWebsiteCategory(WebsiteCategoryRequest websiteCategoryRequest) {
        Optional<WebsiteCategory> byCategoryIgnoreCase = websiteCategoryRepository.findByCategoryIgnoreCase(websiteCategoryRequest.getCategory());
        if (byCategoryIgnoreCase.isPresent()) {
            throw new BadRequestException(ErrorData.WEBSITE_CATEGORY_ALREADY_PRESENT);
        }
        WebsiteCategory websiteCategoryTransformed = websiteCategoryRequestTransformer.transform(websiteCategoryRequest);
        WebsiteCategory websiteCategoryResponse = websiteCategoryRepository.save(websiteCategoryTransformed);
        log.info("Website category saved with response: {}", websiteCategoryResponse);
        return ResponseEntity.ok(websiteCategoryResponseTransformer.transform(websiteCategoryResponse));
    }

    @Override
    public ResponseEntity<ListResponse> getWebsiteCategoryList() {
        List<WebsiteCategoryResponse> websiteCategoryResponses = websiteCategoryRepository.findAll()
                .stream()
                .map(websiteCategoryResponseTransformer::transform)
                .toList();
        ListResponse listResponse = websiteCategoryResponsesToListResponseTransformer.transform(websiteCategoryResponses);
        return ResponseEntity.ok(listResponse);
    }
}
