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
  File: GoogleOauthServiceImplementation
 */

import com.ap.listing.dao.repository.WebsitePublisherRepository;
import com.ap.listing.enums.ErrorData;
import com.ap.listing.exception.BadRequestException;
import com.ap.listing.model.WebsitePublisher;
import com.ap.listing.payload.request.GoogleOauthGa4Request;
import com.ap.listing.payload.response.InitiateGA4OAuthResponse;
import com.ap.listing.processor.GoogleGa4OauthInitiator;
import com.ap.listing.service.GoogleOauthService;
import com.bloggios.provider.payload.ModuleResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Slf4j
public class GoogleOauthServiceImplementation implements GoogleOauthService {

    private final GoogleGa4OauthInitiator googleGa4OauthInitiator;
    private final WebsitePublisherRepository websitePublisherRepository;

    @Override
    public ResponseEntity<InitiateGA4OAuthResponse> initiateOauth(GoogleOauthGa4Request googleOauthGa4Request) {
        WebsitePublisher websitePublisher = websitePublisherRepository.findByPublishingId(googleOauthGa4Request.getPublishingId())
                .orElseThrow(() -> new BadRequestException(ErrorData.WEBSITE_PUBLISHER_NOT_FOUND));
        InitiateGA4OAuthResponse initiateGA4OAuthResponse = googleGa4OauthInitiator.initiate(googleOauthGa4Request, websitePublisher);
        return ResponseEntity.ok(initiateGA4OAuthResponse);
    }
}
