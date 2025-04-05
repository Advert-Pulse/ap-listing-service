package com.ap.listing.transformer;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: WebsiteCategoryRequestTransformer
 */

import com.ap.listing.model.WebsiteCategory;
import com.ap.listing.payload.request.WebsiteCategoryRequest;
import com.ap.listing.utils.SecurityContextUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.Date;

@Component
@Slf4j
public class WebsiteCategoryRequestTransformer {

    public WebsiteCategory transform(WebsiteCategoryRequest websiteCategoryRequest) {
        Date now = new Date();
        String userId = SecurityContextUtil.getLoggedInUserOrThrow().getUserId();
        WebsiteCategory websiteCategory = WebsiteCategory
                .builder()
                .category(websiteCategoryRequest.getCategory().toLowerCase())
                .description(websiteCategoryRequest.getDescription())
                .createdAt(now)
                .updatedAt(now)
                .createdBy(userId)
                .updatedBy(userId)
                .build();
        log.info("Website category transformed: {}", websiteCategory);
        return websiteCategory;
    }
}
