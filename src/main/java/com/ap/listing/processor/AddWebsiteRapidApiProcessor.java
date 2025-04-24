package com.ap.listing.processor;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: AddWebsiteRapidApiProcessor
 */

import com.ap.listing.feign.AhrefFeignClient;
import com.ap.listing.model.Website;
import com.ap.listing.payload.response.AhrefWebsiteTrafficResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
@Slf4j
public class AddWebsiteRapidApiProcessor {

    private final AhrefFeignClient ahrefFeignClient;

    @Async
    public void process(String website) {
        log.info("Add Website Rapid Api Processor: {}", website);
        AhrefWebsiteTrafficResponse ahrefWebsiteTraffic = ahrefFeignClient.getWebsiteTraffic(website);
        log.info("Ahref Website Traffic Response: {}", ahrefWebsiteTraffic);
    }
}
