package com.ap.listing.service;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: PreferenceService
 */

import com.ap.listing.payload.response.PreferenceResponse;
import com.bloggios.provider.payload.ModuleResponse;
import org.springframework.http.ResponseEntity;

public interface PreferenceService {

    ResponseEntity<ModuleResponse> addPreference(String preference);
    ResponseEntity<PreferenceResponse> getPreference();
}
