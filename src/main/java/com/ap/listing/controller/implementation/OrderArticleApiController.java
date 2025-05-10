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
import com.ap.listing.service.OrderArticleService;
import com.bloggios.provider.payload.ModuleResponse;
import com.bloggios.provider.utils.ControllerHelper;
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
}
