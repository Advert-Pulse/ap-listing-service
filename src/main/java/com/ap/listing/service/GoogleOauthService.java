package com.ap.listing.service;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: GoogleOauthService
 */

import com.ap.listing.payload.request.GoogleOauthGa4Request;
import com.bloggios.provider.payload.ModuleResponse;
import org.springframework.http.ResponseEntity;

public interface GoogleOauthService {
    ResponseEntity<ModuleResponse> initiateOauth(GoogleOauthGa4Request googleOauthGa4Request);
}
