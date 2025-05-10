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
package com.ap.listing.controller.implementation;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: OrderArticleApiController
 */

import com.ap.listing.constants.ApiConstants;
import com.ap.listing.controller.OrderArticleApi;
import com.ap.listing.payload.request.OrderArticleRequest;
import com.ap.listing.payload.response.ListResponse;
import com.ap.listing.service.OrderArticleService;
import com.bloggios.provider.payload.ModuleResponse;
import com.bloggios.provider.utils.ControllerHelper;
import com.bloggios.query.payload.ListPayload;
import lombok.RequiredArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequiredArgsConstructor
public class OrderArticleApiController implements OrderArticleApi {

    private static final Logger LOGGER = LoggerFactory.getLogger(OrderArticleApiController.class);

    private final OrderArticleService orderArticleService;

    @Override
    public ResponseEntity<ModuleResponse> addOrderArticle(OrderArticleRequest orderArticleRequest) {
        return ControllerHelper.loggedResponse(
                ()-> orderArticleService.addOrderArticle(orderArticleRequest),
                ApiConstants.ADD_ORDER_ARTICLE,
                LOGGER
        );
    }

    @Override
    public ResponseEntity<ListResponse> myList(ListPayload listPayload) {
        return ControllerHelper.loggedResponse(
                ()-> orderArticleService.myList(listPayload),
                ApiConstants.ORDER_ARTICLE_MY_LIST,
                LOGGER
        );
    }
}
