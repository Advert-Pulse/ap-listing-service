package com.ap.listing.transformer;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: WebsiteToWebsiteResponseTransformer
 */

import com.ap.listing.model.Website;
import com.ap.listing.payload.response.WebsiteResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
@Slf4j
public class WebsiteToWebsiteResponseTransformer {

    public WebsiteResponse transform(Website website) {
        WebsiteResponse websiteResponse = WebsiteResponse
                .builder()
                .domain(website.getDomain())
                .isAvailable(website.getIsAvailable())
                .dateCreated(website.getDateCreated())
                .dateUpdated(website.getDateUpdated())
                .userId(website.getUserId())
                .build();
        log.info("WebsiteToWebsiteResponseTransformer transformed: {}", websiteResponse);
        return websiteResponse;
    }
}
