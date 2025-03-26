package com.ap.listing.service.implementation;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: WebsitePublisherServiceImplementation
 */

import com.ap.listing.payload.request.PublishWebsiteRequest;
import com.ap.listing.service.WebsitePublisherService;
import com.bloggios.provider.payload.ModuleResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
@Slf4j
public class WebsitePublisherServiceImplementation implements WebsitePublisherService {

    @Override
    public ResponseEntity<ModuleResponse> publishSite(PublishWebsiteRequest publishWebsiteRequest, String websiteId) {
        return null;
    }
}
