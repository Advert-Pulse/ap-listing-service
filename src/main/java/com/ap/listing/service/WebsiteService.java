package com.ap.listing.service;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: WebsiteService
 */

import com.ap.listing.payload.response.AddWebsiteResponse;
import com.bloggios.provider.payload.ModuleResponse;
import org.springframework.http.ResponseEntity;

import java.util.List;

public interface WebsiteService {

    ResponseEntity<AddWebsiteResponse> addWebsite(String website);
    ResponseEntity<ModuleResponse> addMultipleWebsite(List<String> websites);
}
