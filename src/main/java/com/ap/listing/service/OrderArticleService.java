package com.ap.listing.service;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: OrderArticleService
 */

import com.ap.listing.model.OrderArticle;
import com.ap.listing.payload.request.OrderArticleRequest;
import com.bloggios.provider.payload.ModuleResponse;
import org.springframework.http.ResponseEntity;

public interface OrderArticleService {

    ResponseEntity<ModuleResponse> addOrderArticle(OrderArticleRequest orderArticleRequest);
}
