/**
 * Proprietary License Agreement
 * <p>
 * Copyright (c) 2025 Advert Pulse
 * <p>
 * This software is the confidential and proprietary property of Advert Pulse
 * and is licensed, not sold. The application owner is Advert Pulse, and the
 * developer and maintainer is Bloggios. Only authorized Bloggios administrators
 * are permitted to copy, modify, distribute, or sublicense this software under
 * the terms set forth in this agreement.
 * <p>
 * You may not:
 * 1. Copy, modify, distribute, or sublicense this software without express
 *    written permission from Advert Pulse or Bloggios.
 * 2. Reverse engineer, decompile, disassemble, or otherwise attempt to derive
 *    the source code of the software.
 * 3. Modify this license in any way, including but not limited to altering its
 *    terms, even by Advert Pulse or any other entity, without express written
 *    permission from Bloggios administrators. Bloggios is the creator of this
 *    license and retains exclusive rights to update or modify it.
 * 4. Update or modify the license without written permission from Bloggios
 *    administrators.
 * <p>
 * The software is provided "as is," and Advert Pulse makes no warranties,
 * express or implied, regarding the software, including but not limited to any
 * warranties of merchantability, fitness for a particular purpose, or
 * non-infringement.
 * <p>
 * For inquiries regarding licensing, please contact support@bloggios.com.
 */
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
import com.ap.listing.payload.response.ListResponse;
import com.ap.listing.payload.response.OrderArticleResponse;
import com.ap.listing.processor.UserIdAdditionInFilter;
import com.ap.listing.properties.OrderArticleListProperties;
import com.ap.listing.service.OrderArticleService;
import com.ap.listing.transformer.OrderArticleRequestToOrderArticleTransformer;
import com.ap.listing.transformer.OrderArticleToOrderArticleResponseTransformer;
import com.ap.listing.validator.NoUserIdInFilterValidator;
import com.bloggios.provider.payload.ModuleResponse;
import com.bloggios.query.payload.ListPayload;
import com.bloggios.query.processor.ListProcessor;
import com.bloggios.query.query.InitQuery;
import jakarta.persistence.TypedQuery;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
@Slf4j
public class OrderArticleServiceImplementation implements OrderArticleService {

    private final OrderArticleRequestToOrderArticleTransformer orderArticleRequestToOrderArticleTransformer;
    private final OrderArticleRepository orderArticleRepository;
    private final NoUserIdInFilterValidator noUserIdInFilterValidator;
    private final UserIdAdditionInFilter userIdAdditionInFilter;
    private final ListProcessor listProcessor;
    private final OrderArticleListProperties orderArticleListProperties;
    private final InitQuery<OrderArticle> initQuery;
    private final OrderArticleToOrderArticleResponseTransformer orderArticleToOrderArticleResponseTransformer;

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

    @Override
    public ResponseEntity<ListResponse> myList(ListPayload listPayload) {
        noUserIdInFilterValidator.validate(listPayload);
        ListPayload processedListPayload = userIdAdditionInFilter.process(listPayload);
        ListPayload transformedListPayload = listProcessor.initProcess(processedListPayload, orderArticleListProperties.getData(), "dateUpdated");
        TypedQuery<OrderArticle> build = initQuery.build(transformedListPayload, OrderArticle.class);
        List<OrderArticleResponse> websitePublisherResponses = build
                .getResultList()
                .stream()
                .map(orderArticleToOrderArticleResponseTransformer::transform)
                .toList();
        ListResponse listResponse = ListResponse
                .builder()
                .object(websitePublisherResponses)
                .page(listPayload.getPage())
                .size(listPayload.getSize())
                .totalRecordsCount(initQuery.getTotalRecords(transformedListPayload, OrderArticle.class))
                .build();
        return ResponseEntity.ok(listResponse);
    }
}
