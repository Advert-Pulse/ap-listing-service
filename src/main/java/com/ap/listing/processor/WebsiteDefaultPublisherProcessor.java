package com.ap.listing.processor;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: WesbiteDefaultPublisherProcessor
 */

import com.ap.listing.dao.repository.WebsitePublisherRepository;
import com.ap.listing.enums.ErrorData;
import com.ap.listing.exception.BadRequestException;
import com.ap.listing.model.Website;
import com.ap.listing.model.WebsitePublisher;
import com.ap.listing.transformer.DefaultWebsitePublisherTransformer;
import com.ap.listing.utils.SecurityContextUtil;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.Optional;

@Component
@RequiredArgsConstructor
@Slf4j
public class WebsiteDefaultPublisherProcessor {

    private final WebsitePublisherRepository websitePublisherRepository;
    private final DefaultWebsitePublisherTransformer defaultWebsitePublisherTransformer;

    public WebsitePublisher process(Website website) {
        String userId = SecurityContextUtil.getLoggedInUserOrThrow().getUserId();
        Optional<WebsitePublisher> byWebsiteAndUserId = websitePublisherRepository.findByWebsiteAndUserId(website, userId);
        if (byWebsiteAndUserId.isEmpty()) {
            WebsitePublisher websitePublisher = defaultWebsitePublisherTransformer.transform(website);
            WebsitePublisher websitePublisherResponse = websitePublisherRepository.save(websitePublisher);
            log.info("Website Publisher saved: {}", websitePublisherResponse);
            return websitePublisherResponse;
        } else {
            throw new BadRequestException(ErrorData.WEBSITE_ALREADY_ADDED);
        }
    }
}
