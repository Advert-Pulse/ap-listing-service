package com.ap.listing.transformer;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: WebsiteCategoryResponseTransformer
 */

import com.ap.listing.model.WebsiteCategory;
import com.ap.listing.payload.response.WebsiteCategoryResponse;
import com.ap.listing.utils.StringCapitalizer;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class WebsiteCategoryResponseTransformer {

    public WebsiteCategoryResponse transform(WebsiteCategory websiteCategory) {
        WebsiteCategoryResponse websiteCategoryResponse = WebsiteCategoryResponse
                .builder()
                .category(StringCapitalizer.formatText(websiteCategory.getCategory()))
                .description(websiteCategory.getDescription())
                .createdAt(websiteCategory.getCreatedAt())
                .updatedAt(websiteCategory.getUpdatedAt())
                .build();
        log.info("WebsiteCategoryResponseTransformer transform websiteCategory = {}", websiteCategoryResponse);
        return websiteCategoryResponse;
    }
}
