package com.ap.listing.service.implementation;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: OrderArticleServiceImplementation
 */

import com.ap.listing.dao.repository.OrderArticleRepository;
import com.ap.listing.model.OrderArticle;
import com.ap.listing.payload.request.OrderArticleRequest;
import com.ap.listing.service.OrderArticleService;
import com.ap.listing.transformer.OrderArticleRequestToOrderArticleTransformer;
import com.bloggios.provider.payload.ModuleResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.common.Uuid;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@RequiredArgsConstructor
@Slf4j
public class OrderArticleServiceImplementation implements OrderArticleService {

    private final OrderArticleRequestToOrderArticleTransformer orderArticleRequestToOrderArticleTransformer;
    private final OrderArticleRepository orderArticleRepository;

    @Override
    public ResponseEntity<ModuleResponse> addOrderArticle(OrderArticleRequest orderArticleRequest) {
        OrderArticle orderArticle = orderArticleRequestToOrderArticleTransformer.transform(orderArticleRequest);
        OrderArticle orderArticleResponse = orderArticleRepository.save(orderArticle);
        log.info("Order Article Saved to database : {}", orderArticleResponse.toString());
        return ResponseEntity.ok(
                ModuleResponse
                        .builder()
                        .userId(UUID.fromString(orderArticleResponse.getUserId()))
                        .message("Order Article Saved Successfully")
                        .build()
        );
    }
}
