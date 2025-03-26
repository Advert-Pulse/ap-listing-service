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

    public WebsitePublisher transform(PublishWebsiteRequest publishWebsiteRequest, Website websiteData) {
        WebsitePublisher website = new WebsitePublisher();
        String publishingId = publishingIdGenerator.generate();
        website.setPublishingId(publishingId);
        website.setWebsitePublishingStatus(WebsitePublishingStatus.PENDING_MODERATION.name());
        //website.setLanguage(publishWebsiteRequest.getLanguage());
        website.setSpecialRequirements(publishWebsiteRequest.getSpecialRequirements());
        website.setProductSpecialRequirements(publishWebsiteRequest.getProductSpecialRequirements());
        website.setCrowdPlacedContent(publishWebsiteRequest.isCrowdPlacedContent());
        website.setSponsoredContent(publishWebsiteRequest.isSponsoredContent());
        website.setBasicContentSize(publishWebsiteRequest.getBasicContentSize());
        website.setLinkAttribute(publishWebsiteRequest.getLinkAttribute());
        website.setContentPlacementPrice(publishWebsiteRequest.getContentPlacementPrice());
        website.setWritingAndPlacementPrice(publishWebsiteRequest.getWritingAndPlacementPrice());
        website.setExtraSizeContentWriting(publishWebsiteRequest.getExtraSizeContentWriting());
        website.setSpecialTopicPricing(publishWebsiteRequest.getSpecialTopicPricing());
        website.setExtraLinkPricing(publishWebsiteRequest.getExtraLinkPricing());
        website.setLinksToBePlacedInOneArticle(publishWebsiteRequest.getLinksToBePlacedInOneArticle());
        website.setLinkInsertionPrice(publishWebsiteRequest.getLinkInsertionPrice());
        website.setLinkInsertionSpecialTopicPrice(publishWebsiteRequest.getLinkInsertionSpecialTopicPrice());
        website.setBestArticleLinkForGuestPosting(publishWebsiteRequest.getBestArticleLinkForGuestPosting());
        website.setBestArticleLinkForLinkInsertion(publishWebsiteRequest.getBestArticleLinkForLinkInsertion());
        website.setConsiderPlacingBuyerArticleForFree(publishWebsiteRequest.isConsiderPlacingBuyerArticleForFree());
        website.setRequirementForSubmittingFreeArticle(publishWebsiteRequest.getRequirementForSubmittingFreeArticle());
        website.setCategories(getWebsiteCategories(publishWebsiteRequest.getCategories()));
        log.info("{} >> transform -> website: {}", getClass().getSimpleName(), website);
        return website;
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
