package com.ap.listing.processor;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: UrlAvailabilityWebsiteProcessor
 */

import com.ap.listing.dao.repository.WebsiteRepository;
import com.ap.listing.enums.ErrorData;
import com.ap.listing.exception.BadRequestException;
import com.ap.listing.model.Website;
import com.ap.listing.transformer.WebsiteTransformer;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
@Slf4j
public class UrlAvailabilityWebsiteProcessor {

    private final WebsiteTransformer websiteTransformer;
    private final WebsiteRepository websiteRepository;

    public void process(Website website, boolean isUrlAvailable) {
        Boolean isAvailable = website.getIsAvailable();
        if (!isUrlAvailable && Boolean.TRUE.equals(isAvailable)) {
            Website websiteEntity = websiteTransformer.transformWebsiteWithAvailability(website, Boolean.FALSE);
            Website websiteResponse = websiteRepository.save(websiteEntity);
            log.info("Website updated for availability: {}", websiteResponse);
            throw new BadRequestException(ErrorData.WEBSITE_IRRESPONSIVE, "website");
        } else if (isUrlAvailable && Boolean.FALSE.equals(isAvailable)) {
            Website websiteEntity = websiteTransformer.transformWebsiteWithAvailability(website, Boolean.TRUE);
            Website websiteResponse = websiteRepository.save(websiteEntity);
            log.info("Website updated for availability: {}", websiteResponse);
        }
    }
}
