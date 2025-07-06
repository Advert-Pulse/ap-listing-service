package com.ap.listing.controller;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: GoogleOauthController
 */

import com.ap.listing.constants.ApiConstants;
import com.ap.listing.payload.request.GoogleOauthGa4Request;
import com.ap.listing.service.GoogleOauthService;
import com.bloggios.provider.payload.ModuleResponse;
import com.bloggios.provider.utils.ControllerHelper;
import lombok.RequiredArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/v1/google-oauth")
@RequiredArgsConstructor
public class GoogleOauthController {

    private static final Logger LOGGER = LoggerFactory.getLogger(GoogleOauthController.class);
    private final GoogleOauthService googleOauthService;

    @PostMapping
    public ResponseEntity<ModuleResponse> initiateOauth(@RequestBody GoogleOauthGa4Request googleOauthGa4Request) {
        return ControllerHelper.loggedResponse(
                () -> googleOauthService.initiateOauth(googleOauthGa4Request),
                ApiConstants.INITIAL_GOOGLE_OAUTH,
                LOGGER
        );
    }
}
