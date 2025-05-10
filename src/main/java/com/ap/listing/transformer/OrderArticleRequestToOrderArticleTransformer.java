package com.ap.listing.transformer;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: OrderArticleRequestToOrderArticleTransformer
 */

import com.ap.listing.model.OrderArticle;
import com.ap.listing.payload.request.OrderArticleRequest;
import com.ap.listing.utils.SecurityContextUtil;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.stereotype.Component;

import java.util.Date;

@Component
@RequiredArgsConstructor
@Slf4j
public class OrderArticleRequestToOrderArticleTransformer {

    private final ModelMapper modelMapper;

    public OrderArticle transform(OrderArticleRequest orderArticleRequest) {
        Date now = new Date();
        OrderArticle orderArticle = modelMapper.map(orderArticleRequest, OrderArticle.class);
        orderArticle.setDateCreated(now);
        orderArticle.setDateUpdated(now);
        orderArticle.setUserId(SecurityContextUtil.getLoggedInUserOrThrow().getUserId());
        log.info("Order article transformed: {}", orderArticle);
        return orderArticle;
    }
}
