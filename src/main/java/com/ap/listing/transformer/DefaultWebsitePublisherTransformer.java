package com.ap.listing.transformer;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: DefaultWebsitePublisherTransformer
 */

import com.ap.listing.model.Website;
import com.ap.listing.model.WebsitePublisher;
import com.ap.listing.utils.SecurityContextUtil;
import org.springframework.stereotype.Component;

import java.util.Date;

@Component
public class DefaultWebsitePublisherTransformer {

    public WebsitePublisher transform(Website website) {
        Date now = new Date();
        return WebsitePublisher
                .builder()
                .website(website)
                .userId(SecurityContextUtil.getLoggedInUserOrThrow().getUserId())
                .dateCreated(now)
                .dateUpdated(now)
                .build();
    }
}
