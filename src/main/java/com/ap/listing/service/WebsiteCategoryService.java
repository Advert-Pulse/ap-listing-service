package com.ap.listing.service;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: WebsiteCategory
 */

import com.ap.listing.payload.request.WebsiteCategoryRequest;
import com.ap.listing.payload.response.ListResponse;
import com.ap.listing.payload.response.WebsiteCategoryResponse;
import org.springframework.http.ResponseEntity;

public interface WebsiteCategoryService {

    ResponseEntity<WebsiteCategoryResponse> addWebsiteCategory(WebsiteCategoryRequest websiteCategoryRequest);
    ResponseEntity<ListResponse> getWebsiteCategoryList();
}
