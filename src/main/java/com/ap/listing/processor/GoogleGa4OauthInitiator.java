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
