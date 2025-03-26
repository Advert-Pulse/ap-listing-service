package com.ap.listing.service;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: WebsitePublisherService
 */

import com.ap.listing.payload.request.PublishWebsiteRequest;
import com.bloggios.provider.payload.ModuleResponse;
import org.springframework.http.ResponseEntity;

public interface WebsitePublisherService {

    ResponseEntity<ModuleResponse> publishSite(PublishWebsiteRequest publishWebsiteRequest, String websiteId);
}
