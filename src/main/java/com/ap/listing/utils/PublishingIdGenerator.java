package com.ap.listing.utils;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: PublishingIdGenerator
 */

import com.ap.listing.dao.repository.WebsitePublisherRepository;
import com.bloggios.provider.utils.RandomGenerator;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
@Slf4j
public class PublishingIdGenerator {

    private static final String GENERATOR_DATA = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

    private final WebsitePublisherRepository websitePublisherRepository;

    public String generate() {
        log.info("{} >> generate -> Generating Publishing Id", getClass().getSimpleName());
        String sb;
        int limit = 0;
        do {
            sb =
                    "AP00" +
                            RandomGenerator.generateRandomString(GENERATOR_DATA, 4) +
                            RandomGenerator.generateRandomString(GENERATOR_DATA, 4);
            limit++;
            log.info("{} >> generate -> Limit: {}, publishingId: {}", getClass().getSimpleName(), limit, sb);
        } while (websitePublisherRepository.findByPublishingId(sb).isPresent() || limit < 6);
        return sb;
    }
}
