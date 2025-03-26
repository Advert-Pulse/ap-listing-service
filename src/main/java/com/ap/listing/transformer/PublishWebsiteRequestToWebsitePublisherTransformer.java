package com.ap.listing.transformer;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: PublishWebsiteRequestToWebsitePublisherTransformer
 */

import com.ap.listing.dao.repository.WebsiteCategoryRepository;
import com.ap.listing.enums.WebsitePublishingStatus;
import com.ap.listing.model.Website;
import com.ap.listing.model.WebsiteCategory;
import com.ap.listing.model.WebsitePublisher;
import com.ap.listing.payload.request.PublishWebsiteRequest;
import com.ap.listing.utils.PublishingIdGenerator;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

@Component
@RequiredArgsConstructor
@Slf4j
public class PublishWebsiteRequestToWebsitePublisherTransformer {

    private final PublishingIdGenerator publishingIdGenerator;
    private final WebsiteCategoryRepository websiteCategoryRepository;

    public WebsitePublisher transform(PublishWebsiteRequest publishWebsiteRequest, Website website) {
        WebsitePublisher websitePublisher = new WebsitePublisher();
        String publishingId = publishingIdGenerator.generate();
        websitePublisher.setPublishingId(publishingId);
        websitePublisher.setWebsitePublishingStatus(WebsitePublishingStatus.PENDING_MODERATION.name());
        //websitePublisher.setLanguage(publishWebsiteRequest.getLanguage());
        websitePublisher.setSpecialRequirements(publishWebsiteRequest.getSpecialRequirements());
        websitePublisher.setProductSpecialRequirements(publishWebsiteRequest.getProductSpecialRequirements());
        websitePublisher.setCrowdPlacedContent(publishWebsiteRequest.isCrowdPlacedContent());
        websitePublisher.setSponsoredContent(publishWebsiteRequest.isSponsoredContent());
        websitePublisher.setBasicContentSize(publishWebsiteRequest.getBasicContentSize());
        websitePublisher.setLinkAttribute(publishWebsiteRequest.getLinkAttribute());
        websitePublisher.setContentPlacementPrice(publishWebsiteRequest.getContentPlacementPrice());
        websitePublisher.setWritingAndPlacementPrice(publishWebsiteRequest.getWritingAndPlacementPrice());
        websitePublisher.setExtraSizeContentWriting(publishWebsiteRequest.getExtraSizeContentWriting());
        websitePublisher.setSpecialTopicPricing(publishWebsiteRequest.getSpecialTopicPricing());
        websitePublisher.setExtraLinkPricing(publishWebsiteRequest.getExtraLinkPricing());
        websitePublisher.setLinksToBePlacedInOneArticle(publishWebsiteRequest.getLinksToBePlacedInOneArticle());
        websitePublisher.setLinkInsertionPrice(publishWebsiteRequest.getLinkInsertionPrice());
        websitePublisher.setLinkInsertionSpecialTopicPrice(publishWebsiteRequest.getLinkInsertionSpecialTopicPrice());
        websitePublisher.setBestArticleLinkForGuestPosting(publishWebsiteRequest.getBestArticleLinkForGuestPosting());
        websitePublisher.setBestArticleLinkForLinkInsertion(publishWebsiteRequest.getBestArticleLinkForLinkInsertion());
        websitePublisher.setConsiderPlacingBuyerArticleForFree(publishWebsiteRequest.isConsiderPlacingBuyerArticleForFree());
        websitePublisher.setRequirementForSubmittingFreeArticle(publishWebsiteRequest.getRequirementForSubmittingFreeArticle());
        websitePublisher.setCategories(getWebsiteCategories(publishWebsiteRequest.getCategories()));
        websitePublisher.setWebsite(website);
        log.info("{} >> transform -> websitePublisher: {}", getClass().getSimpleName(), websitePublisher);
        return websitePublisher;
    }

    private List<WebsiteCategory> getWebsiteCategories(List<String> categories) {
        if (Objects.nonNull(categories)) {
            List<WebsiteCategory> websiteCategories = new ArrayList<>();
            categories
                    .forEach(category -> websiteCategoryRepository.findByCategoryIgnoreCase(category)
                            .ifPresent(websiteCategories::add));
            return websiteCategories;
        }
        return Collections.emptyList();
    }
}
