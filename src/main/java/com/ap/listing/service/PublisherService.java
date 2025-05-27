package com.ap.listing.service;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: PublisherService
 */

import com.bloggios.provider.payload.ModuleResponse;
import org.springframework.http.ResponseEntity;

public interface PublisherService {
    ResponseEntity<ModuleResponse> manageTaskInitial(String taskId, String status);
}
