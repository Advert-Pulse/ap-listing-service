package com.ap.listing.service.implementation;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: OrderArticleServiceImplementation
 */

import com.ap.listing.payload.request.OrderArticleRequest;
import com.ap.listing.service.OrderArticleService;
import com.bloggios.provider.payload.ModuleResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
@Slf4j
public class OrderArticleServiceImplementation implements OrderArticleService {

    @Override
    public ResponseEntity<ModuleResponse> addOrderArticle(OrderArticleRequest orderArticleRequest) {
        return null;
    }
}
