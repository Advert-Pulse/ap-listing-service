package com.ap.listing.service.implementation;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: GoogleOauthServiceImplementation
 */

import com.ap.listing.payload.request.GoogleOauthGa4Request;
import com.ap.listing.payload.request.GoogleRefreshTokenResponse;
import com.ap.listing.processor.GoogleGa4OauthInitiator;
import com.ap.listing.service.GoogleOauthService;
import com.bloggios.provider.payload.ModuleResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
@Slf4j
public class GoogleOauthServiceImplementation implements GoogleOauthService {

    private final GoogleGa4OauthInitiator googleGa4OauthInitiator;

    @Override
    public ResponseEntity<ModuleResponse> initiateOauth(GoogleOauthGa4Request googleOauthGa4Request) {
        GoogleRefreshTokenResponse tokenResponse = googleGa4OauthInitiator.getToken(googleOauthGa4Request);

        return null;
    }
}
