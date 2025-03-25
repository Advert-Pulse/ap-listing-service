package com.ap.listing.transformer;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: WebsiteTransformer
 */

import com.ap.listing.model.DomainMetrics;
import com.ap.listing.model.Website;
import com.ap.listing.payload.response.DomainMetricsFeignResponse;
import com.ap.listing.utils.SecurityContextUtil;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;

import java.util.Date;

@Component
public class WebsiteTransformer {

    public Website transform(String website) {
        return Website
                .builder()
                .domain(website)
                .isAvailable(Boolean.TRUE)
                .dateCreated(new Date())
                .userId(SecurityContextUtil.getLoggedInUserOrThrow().getUserId())
                .build();
    }

    public Website transformWebsiteNotAvailable(Website website) {
        website.setIsAvailable(Boolean.FALSE);
        return website;
    }
}
