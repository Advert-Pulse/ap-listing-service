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
import com.ap.listing.model.Website;
import com.ap.listing.model.WebsiteCategory;
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
        Website website = websiteRepository.findById(websitePublisher.getWebsite().getWebsiteId())
                .orElseThrow(() -> new BadRequestException(ErrorData.WEBSITE_NOT_FOUND_BY_ID));
        processPrices(websitePublisher, website);
        processLinkAttribute(websitePublisher, website);
        String requiredContentSize = updateMinContentSize(websitePublisher.getBasicContentSize(), website.getBasicContentSize());
        website.setBasicContentSize(requiredContentSize);
        processOwner(websitePublisher, website);
        processTat(websitePublisher, website);
        processExampleOfWork(websitePublisher, website);
        processSponsoredContent(websitePublisher, website);
        processContentPlacement(websitePublisher, website);
        processWritingPlacement(websitePublisher, website);
        processCategories(websitePublisher, website);
        log.info("WebsitePublisherToWebsiteSyncProcessor >> Update website : {}", website);
        Website websiteResponse = websiteRepository.save(website);
        log.info("WebsitePublisherToWebsiteSyncProcessor saved successfully: {}", websiteResponse);
    }

    private void processCategories(WebsitePublisher websitePublisher, Website website) {
        if (Objects.isNull(website.getCategories())) {
            website.setCategories(websitePublisher.getCategories());
        } else {
            List<WebsiteCategory> categories = website.getCategories();
            if (Objects.nonNull(websitePublisher.getCategories()) && !CollectionUtils.isEmpty(categories)) {
                List<WebsiteCategory> websiteCategories = Stream.concat(
                                categories.stream(),
                                websitePublisher.getCategories().stream()
                        )
                        .distinct()
                        .toList();
                website.setCategories(websiteCategories);
            }
        }
    }

    private void processWritingPlacement(WebsitePublisher websitePublisher, Website website) {
        if (Objects.nonNull(websitePublisher.getContentPlacementPrice()) && websitePublisher.getContentPlacementPrice() > 4) {
            website.setContentPlacement(true);
        }
    }

    private void processContentPlacement(WebsitePublisher websitePublisher, Website website) {
        if (Objects.nonNull(websitePublisher.getWritingAndPlacementPrice()) && websitePublisher.getWritingAndPlacementPrice() > 4) {
            website.setWritingPlacement(true);
        }
    }

    private void processSponsoredContent(WebsitePublisher websitePublisher, Website website) {
        if (websitePublisher.isSponsoredContent()) {
            website.setSponsoredContent(true);
        }
    }

    private void processExampleOfWork(WebsitePublisher websitePublisher, Website website) {
        if (Objects.nonNull(websitePublisher.getBestArticleLinkForGuestPosting()) && !CollectionUtils.isEmpty(websitePublisher.getBestArticleLinkForGuestPosting())) {
            website.setExampleOfWork(true);
        }
    }

    private void processTat(WebsitePublisher websitePublisher, Website website) {
        Integer tat = websitePublisher.getTat();
        if (tat != null) {
            if (Objects.nonNull(website.getTat())) {
                Integer min = Math.min(website.getTat(), tat);
                website.setTat(min);
            } else {
                website.setTat(tat);
            }
        }
    }

    private void processOwner(WebsitePublisher websitePublisher, Website website) {
        String ownershipType = websitePublisher.getOwnershipType();
        if (ownershipType.equalsIgnoreCase("owner")) {
            website.setOwnerAvailable(true);
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

    private void processLinkAttribute(WebsitePublisher websitePublisher, Website website) {
        if (Objects.nonNull(websitePublisher.getLinkAttribute())) {
            if (websitePublisher.getLinkAttribute().equalsIgnoreCase("nofollow")) {
                website.setNoFollow(true);
            }
            if (websitePublisher.getLinkAttribute().equalsIgnoreCase("dofollow")) {
                website.setDoFollow(true);
            }
        }
    }

    private void processPrices(WebsitePublisher websitePublisher, Website website) {
        Double publisherMin = websitePublisher.getMinPrice();
        Double publisherMax = websitePublisher.getMaxPrice();

        if (publisherMin != null) {
            if (website.getMinPrice() != null) {
                Double min = Math.min(website.getMinPrice(), publisherMin);
                website.setMinPrice(min);
            } else {
                website.setMinPrice(publisherMin);
            }
        }

        if (publisherMax != null) {
            if (website.getMaxPrice() != null) {
                Double max = Math.max(website.getMaxPrice(), publisherMax);
                website.setMaxPrice(max);
            } else {
                website.setMaxPrice(publisherMax);
            }
        }
    }
}
