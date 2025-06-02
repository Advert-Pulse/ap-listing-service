package com.ap.listing.service;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: BuyerService
 */

import com.ap.listing.payload.request.BuyerImprovementRequest;
import com.bloggios.provider.payload.ModuleResponse;
import jakarta.servlet.http.HttpServletRequest;
import org.springframework.http.ResponseEntity;

public interface BuyerService {
    ResponseEntity<ModuleResponse> manageImprovement(BuyerImprovementRequest buyerImprovementRequest);
    ResponseEntity<ModuleResponse> manageCompleted(String taskId, HttpServletRequest request);
}
