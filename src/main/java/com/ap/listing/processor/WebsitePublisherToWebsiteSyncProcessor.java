/**
 * Proprietary License Agreement
 * <p>
 * Copyright (c) 2025 Advert Pulse
 * <p>
 * This software is the confidential and proprietary property of Advert Pulse
 * and is licensed, not sold. The application owner is Advert Pulse, and the
 * developer and maintainer is Bloggios. Only authorized Bloggios administrators
 * are permitted to copy, modify, distribute, or sublicense this software under
 * the terms set forth in this agreement.
 * <p>
 * You may not:
 * 1. Copy, modify, distribute, or sublicense this software without express
 *    written permission from Advert Pulse or Bloggios.
 * 2. Reverse engineer, decompile, disassemble, or otherwise attempt to derive
 *    the source code of the software.
 * 3. Modify this license in any way, including but not limited to altering its
 *    terms, even by Advert Pulse or any other entity, without express written
 *    permission from Bloggios administrators. Bloggios is the creator of this
 *    license and retains exclusive rights to update or modify it.
 * 4. Update or modify the license without written permission from Bloggios
 *    administrators.
 * <p>
 * The software is provided "as is," and Advert Pulse makes no warranties,
 * express or implied, regarding the software, including but not limited to any
 * warranties of merchantability, fitness for a particular purpose, or
 * non-infringement.
 * <p>
 * For inquiries regarding licensing, please contact support@bloggios.com.
 */
package com.ap.listing.processor;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: WebsitePublisherToWebsiteSyncProcessor
 */

import com.ap.listing.dao.repository.WebsiteRepository;
import com.ap.listing.enums.ErrorData;
import com.ap.listing.exception.BadRequestException;
import com.ap.listing.model.WebsiteCategory;
import com.ap.listing.model.WebsiteData;
import com.ap.listing.model.WebsitePublisher;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.Objects;
import java.util.stream.Stream;

@Component
@RequiredArgsConstructor
@Slf4j
public class WebsitePublisherToWebsiteSyncProcessor {

    private static final List<String> PRIORITY_ORDER = List.of("500", "1000", "1500", "2000+");

    private final WebsiteRepository websiteRepository;

    public void doSync(WebsitePublisher websitePublisher) {
        WebsiteData websiteData = websiteRepository.findById(websitePublisher.getWebsiteData().getWebsiteId())
                .orElseThrow(() -> new BadRequestException(ErrorData.WEBSITE_NOT_FOUND_BY_ID));
        processPrices(websitePublisher, websiteData);
        processLinkAttribute(websitePublisher, websiteData);
        String requiredContentSize = updateMinContentSize(websitePublisher.getBasicContentSize(), websiteData.getBasicContentSize());
        websiteData.setBasicContentSize(requiredContentSize);
        processOwner(websitePublisher, websiteData);
        processTat(websitePublisher, websiteData);
        processExampleOfWork(websitePublisher, websiteData);
        processSponsoredContent(websitePublisher, websiteData);
        processContentPlacement(websitePublisher, websiteData);
        processWritingPlacement(websitePublisher, websiteData);
        processCategories(websitePublisher, websiteData);
        log.info("WebsitePublisherToWebsiteSyncProcessor >> Update website : {}", websiteData);
        WebsiteData websiteDataResponse = websiteRepository.save(websiteData);
        log.info("WebsitePublisherToWebsiteSyncProcessor saved successfully: {}", websiteDataResponse);
    }

    private void processCategories(WebsitePublisher websitePublisher, WebsiteData websiteData) {
        if (Objects.isNull(websiteData.getCategories())) {
            websiteData.setCategories(websitePublisher.getCategories());
        } else {
            List<WebsiteCategory> categories = websiteData.getCategories();
            if (Objects.nonNull(websitePublisher.getCategories()) && !CollectionUtils.isEmpty(categories)) {
                List<WebsiteCategory> websiteCategories = Stream.concat(
                                categories.stream(),
                                websitePublisher.getCategories().stream()
                        )
                        .distinct()
                        .toList();
                websiteData.setCategories(websiteCategories);
            }
        }
    }

    private void processContentPlacement(WebsitePublisher websitePublisher, WebsiteData websiteData) {
        if (Objects.nonNull(websitePublisher.getContentPlacementPrice()) && websitePublisher.getContentPlacementPrice() > 4) {
            websiteData.setIsContentPlacement("true");
            if (Objects.isNull(websiteData.getContentPlacementPrice())) {
                websiteData.setContentPlacementPrice(websitePublisher.getContentPlacementPrice());
            } else {
                websiteData.setContentPlacementPrice(Math.min(websitePublisher.getContentPlacementPrice(), websiteData.getContentPlacementPrice()));
            }
        } else {
            websiteData.setIsContentPlacement("false");
        }
    }

    private void processWritingPlacement(WebsitePublisher websitePublisher, WebsiteData websiteData) {
        if (Objects.nonNull(websitePublisher.getWritingAndPlacementPrice()) && websitePublisher.getWritingAndPlacementPrice() > 4) {
            websiteData.setIsWritingPlacement("true");
            if (Objects.isNull(websiteData.getWritingAndPlacementPrice())) {
                websiteData.setWritingAndPlacementPrice(websitePublisher.getWritingAndPlacementPrice());
            } else {
                websiteData.setWritingAndPlacementPrice(Math.min(websitePublisher.getWritingAndPlacementPrice(), websiteData.getWritingAndPlacementPrice()));
            }
        } else {
            websiteData.setIsWritingPlacement("false");
        }
    }

    private void processSponsoredContent(WebsitePublisher websitePublisher, WebsiteData websiteData) {
        if (websitePublisher.isSponsoredContent()) {
            websiteData.setIsSponsoredContent("true");
        } else {
            websiteData.setIsSponsoredContent("false");
        }
    }

    private void processExampleOfWork(WebsitePublisher websitePublisher, WebsiteData websiteData) {
        if (Objects.nonNull(websitePublisher.getBestArticleLinkForGuestPosting()) && !CollectionUtils.isEmpty(websitePublisher.getBestArticleLinkForGuestPosting())) {
            websiteData.setIsExampleOfWork("true");
        } else {
            websiteData.setIsExampleOfWork("false");
        }
    }

    private void processTat(WebsitePublisher websitePublisher, WebsiteData websiteData) {
        Integer tat = websitePublisher.getTat();
        if (tat != null) {
            if (Objects.nonNull(websiteData.getTat())) {
                Integer min = Math.min(websiteData.getTat(), tat);
                websiteData.setTat(min);
            } else {
                websiteData.setTat(tat);
            }
        }
    }

    private void processOwner(WebsitePublisher websitePublisher, WebsiteData websiteData) {
        String ownershipType = websitePublisher.getOwnershipType();
        if (ownershipType.equalsIgnoreCase("owner")) {
            websiteData.setIsOwnerAvailable("true");
        } else {
            websiteData.setIsOwnerAvailable("false");
        }
    }

    public static String updateMinContentSize(String basicContentSize, String minContentSize) {
        if (minContentSize == null || minContentSize.isEmpty()) {
            return basicContentSize;
        }

        int basicPriority = getPriority(basicContentSize);
        int minPriority = getPriority(minContentSize);
        if (basicPriority < minPriority) {
            return basicContentSize;
        }
        return minContentSize;
    }

    private static int getPriority(String contentSize) {
        int index = PRIORITY_ORDER.indexOf(contentSize);
        return index != -1 ? index : Integer.MAX_VALUE;
    }

    private void processLinkAttribute(WebsitePublisher websitePublisher, WebsiteData websiteData) {
        if (Objects.nonNull(websitePublisher.getLinkAttribute())) {
            if (websitePublisher.getLinkAttribute().equalsIgnoreCase("nofollow")) {
                websiteData.setIsNoFollow("true");
            } else {
                websiteData.setIsNoFollow("false");
            }
            if (websitePublisher.getLinkAttribute().equalsIgnoreCase("dofollow")) {
                websiteData.setIsDoFollow("true");
            } else {
                websiteData.setIsDoFollow("false");
            }
        }
    }

    private void processPrices(WebsitePublisher websitePublisher, WebsiteData websiteData) {
        Double publisherMin = websitePublisher.getMinPrice();
        Double publisherMax = websitePublisher.getMaxPrice();

        if (publisherMin != null) {
            if (websiteData.getMinPrice() != null) {
                Double min = Math.min(websiteData.getMinPrice(), publisherMin);
                websiteData.setMinPrice(min);
            } else {
                websiteData.setMinPrice(publisherMin);
            }
        }

        if (publisherMax != null) {
            if (websiteData.getMaxPrice() != null) {
                Double max = Math.max(websiteData.getMaxPrice(), publisherMax);
                websiteData.setMaxPrice(max);
            } else {
                websiteData.setMaxPrice(publisherMax);
            }
        }
    }
}
