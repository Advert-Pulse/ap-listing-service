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
  File: GoogleGa4OauthInitiator
 */

import com.ap.listing.enums.ErrorData;
import com.ap.listing.exception.BadRequestException;
import com.ap.listing.feign.GoogleFeignClient;
import com.ap.listing.payload.request.GoogleOauthGa4Request;
import com.ap.listing.payload.request.GoogleRefreshTokenResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;

@Component
@RequiredArgsConstructor
@Slf4j
public class GoogleGa4OauthInitiator {

    private final GoogleFeignClient googleFeignClient;
    @Value("${feign-client.google-service.client-id}")
    private String clientId;

    @Value("${feign-client.google-service.client-secret}")
    private String clientSecret;

    public GoogleRefreshTokenResponse getToken(GoogleOauthGa4Request googleOAuthRequest) {
        MultiValueMap<String, String> formParams = new LinkedMultiValueMap<>();
        formParams.add("client_id", clientId);
        formParams.add("client_secret", clientSecret);
        formParams.add("code", googleOAuthRequest.getCode());
        formParams.add("grant_type", "authorization_code");
        formParams.add("redirect_uri", googleOAuthRequest.getRedirectUri());
        try {
            return googleFeignClient.exchangeAuthCodeForToken(formParams);
        } catch (Exception e) {
            throw new BadRequestException(ErrorData.GOOGLE_AUTH_TOKEN_EXCHANGE_ERROR, "Error Occurred while fetching the token response through auth code", e.getLocalizedMessage());
        }
    }
}
