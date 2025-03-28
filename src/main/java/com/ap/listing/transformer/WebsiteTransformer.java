package com.ap.listing.transformer;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: WebsiteTransformer
 */

import com.ap.listing.model.Website;
import com.ap.listing.utils.SecurityContextUtil;
import org.springframework.stereotype.Component;

import java.util.Date;

@Component
public class WebsiteTransformer {

    public Website transform(String website) {
        Date now = new Date();
        return Website
                .builder()
                .domain(website)
                .isAvailable(Boolean.TRUE)
                .dateCreated(now)
                .dateUpdated(now)
                .userId(SecurityContextUtil.getLoggedInUserOrThrow().getUserId())
                .build();
    }

    public Website transformWebsiteWithAvailability(Website website, Boolean availability) {
        website.setIsAvailable(availability);
        website.setDateUpdated(new Date());
        return website;
    }
}
