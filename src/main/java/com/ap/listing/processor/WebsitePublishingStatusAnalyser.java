package com.ap.listing.processor;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: WebsitePublishingStatusAnalyser
 */

import com.ap.listing.enums.WebsitePublishingStatus;
import com.ap.listing.model.WebsitePublisher;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.Objects;

@Component
@RequiredArgsConstructor
@Slf4j
public class WebsitePublishingStatusAnalyser {

    public WebsitePublisher analyse(WebsitePublisher websitePublisher) {
        if (
                Objects.nonNull(websitePublisher.getContentPlacementPrice()) && websitePublisher.getContentPlacementPrice() >= 4 &&
                        Objects.nonNull(websitePublisher.getWritingAndPlacementPrice()) && websitePublisher.getWritingAndPlacementPrice() >= 4
        ) {
            if (websitePublisher.getWebsitePublishingStatus().equals(WebsitePublishingStatus.PENDING_SPECIFICATION.name())) {
                websitePublisher.setWebsitePublishingStatus(WebsitePublishingStatus.PENDING_MODERATION.name());
            }
            return websitePublisher;
        }
        return websitePublisher;
    }
}
