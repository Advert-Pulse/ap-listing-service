package com.ap.listing.transformer;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: DefaultWebsitePublisherTransformer
 */

import com.ap.listing.enums.OwnershipType;
import com.ap.listing.enums.WebsitePublishingStatus;
import com.ap.listing.model.Website;
import com.ap.listing.model.WebsitePublisher;
import com.ap.listing.utils.PublishingIdGenerator;
import com.ap.listing.utils.SecurityContextUtil;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.Date;

@Component
@RequiredArgsConstructor
@Slf4j
public class DefaultWebsitePublisherTransformer {


    private final PublishingIdGenerator publishingIdGenerator;

    public WebsitePublisher transform(Website website) {
        Date now = new Date();
        WebsitePublisher websitePublisher = WebsitePublisher
                .builder()
                .website(website)
                .domain(website.getDomain())
                .publishingId(publishingIdGenerator.generate())
                .websitePublishingStatus(WebsitePublishingStatus.PENDING_SPECIFICATION.name())
                .ownershipType(OwnershipType.CONTRIBUTOR.name())
                .userId(SecurityContextUtil.getLoggedInUserOrThrow().getUserId())
                .dateCreated(now)
                .dateUpdated(now)
                .build();
        log.info("Website Publisher transformed: {}", websitePublisher);
        return websitePublisher;
    }
}
