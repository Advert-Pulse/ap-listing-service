package com.ap.listing.generator;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: AddWebsiteResponseGenerator
 */

import com.ap.listing.model.Website;
import com.ap.listing.model.WebsitePublisher;
import com.ap.listing.payload.response.AddWebsiteResponse;
import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;

@UtilityClass
@Slf4j
public class AddWebsiteResponseGenerator {

    public static ResponseEntity<AddWebsiteResponse> generate(Website website, WebsitePublisher websitePublisher, boolean isNewDomain) {
        AddWebsiteResponse addWebsiteResponse = AddWebsiteResponse
                .builder()
                .websiteId(website.getWebsiteId())
                .domain(website.getDomain())
                .isAvailable(website.getIsAvailable())
                .websitePublisherId(websitePublisher.getWebsitePublisherId())
                .isNewDomain(isNewDomain)
                .build();
        log.info("Response generated for addWebsite : addWebsiteResponse={}", addWebsiteResponse);
        return ResponseEntity.ok(addWebsiteResponse);
    }
}
