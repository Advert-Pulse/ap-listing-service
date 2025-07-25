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

import com.ap.listing.constants.ServiceConstants;
import com.ap.listing.dao.repository.OwnershipDetailsRepository;
import com.ap.listing.enums.ErrorData;
import com.ap.listing.exception.BadRequestException;
import com.ap.listing.model.OwnershipDetails;
import com.ap.listing.payload.response.OwnershipDetailsResponse;
import com.ap.listing.payload.response.VerifyOwnershipResponse;
import com.ap.listing.processor.CreateOrUpdateOwnershipProcessor;
import com.ap.listing.processor.HtmlCodeVerifyOwnershipProcessor;
import com.ap.listing.processor.VerifyOwnershipOwnerAvailableProcessor;
import com.ap.listing.processor.VerifyOwnershipProcessor;
import com.ap.listing.service.OwnershipService;
import com.ap.listing.transformer.OwnershipDetailsToResponseTransformer;
import lombok.RequiredArgsConstructor;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.io.InputStreamResource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.io.FileInputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Objects;

@Service
@RequiredArgsConstructor
@Slf4j
public class OwnershipServiceImplementation implements OwnershipService {

    private final OwnershipDetailsRepository ownershipDetailsRepository;
    private final CreateOrUpdateOwnershipProcessor createOrUpdateOwnershipProcessor;
    private final OwnershipDetailsToResponseTransformer ownershipDetailsToResponseTransformer;
    private final VerifyOwnershipProcessor verifyOwnershipProcessor;
    private final VerifyOwnershipOwnerAvailableProcessor verifyOwnershipOwnerAvailableProcessor;
    private final HtmlCodeVerifyOwnershipProcessor htmlCodeVerifyOwnershipProcessor;

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

    @Override
    @SneakyThrows(value = IOException.class)
    public ResponseEntity<InputStreamResource> createAndDownloadOwnershipDetails(String publishingId) {
        OwnershipDetailsResponse ownershipDetailsResponse = this.createOrGetOwnershipDetails(publishingId).getBody();
        Path filePath = Paths.get(ServiceConstants.FILE_DIRECTORY, ServiceConstants.OWNERSHIP_FILE_NAME);
        Files.write(filePath, ownershipDetailsResponse.getUniqueId().getBytes());
        InputStreamResource inputStreamResource = new InputStreamResource(new FileInputStream(filePath.toFile()));
        return ResponseEntity.ok()
                .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=" + ServiceConstants.OWNERSHIP_FILE_NAME)
                .contentLength(Files.size(filePath))
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .body(inputStreamResource);
    }

    @Override
    public ResponseEntity<VerifyOwnershipResponse> verifyOwnership(String publishingId) {
        OwnershipDetails ownershipDetails = ownershipDetailsRepository.findByPublishingId(publishingId)
                .orElseThrow(() -> new BadRequestException(ErrorData.OWNERSHIP_DETAILS_NOT_FOUND));
        String finalLink = ownershipDetails.getFinalLink();
        String linkData = verifyOwnershipProcessor.getLinkData(finalLink);
        if (Objects.isNull(linkData))
            throw new BadRequestException(
                    ErrorData.NULL_RESPONSE_VERIFY_LINK_DATA,
                    "link",
                    "There is no static content found for link: " + finalLink
            );
        if (!linkData.equals(ownershipDetails.getUniqueId()))
            throw new BadRequestException(ErrorData.VERIFY_WEBSITE_FAILURE_INVALID_DATA_PRESENT);
        verifyOwnershipOwnerAvailableProcessor.process(publishingId);
        return ResponseEntity.ok(
                VerifyOwnershipResponse
                        .builder()
                        .isOwner(true)
                        .domain(ownershipDetails.getDomain())
                        .websiteId(ownershipDetails.getWebsiteId())
                        .publishingId(ownershipDetails.getPublishingId())
                        .build()
        );
    }

    @Override
    public ResponseEntity<VerifyOwnershipResponse> verifyOwnershipUsingHtmlCode(String publishingId) {
        OwnershipDetails ownershipDetails = ownershipDetailsRepository.findByPublishingId(publishingId)
                .orElseThrow(() -> new BadRequestException(ErrorData.OWNERSHIP_DETAILS_NOT_FOUND));
        boolean isAvailable = htmlCodeVerifyOwnershipProcessor.checkForCode(ownershipDetails.getDomain(), ownershipDetails.getUniqueId());
        if (!isAvailable) {
            throw new BadRequestException(ErrorData.HTML_DATA_VERIFICATION_ERROR);
        }
        verifyOwnershipOwnerAvailableProcessor.process(publishingId);
        return ResponseEntity.ok(
                VerifyOwnershipResponse
                        .builder()
                        .isOwner(true)
                        .domain(ownershipDetails.getDomain())
                        .websiteId(ownershipDetails.getWebsiteId())
                        .publishingId(ownershipDetails.getPublishingId())
                        .build()
        );
    }
}
