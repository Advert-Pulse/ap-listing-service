package com.ap.listing.transformer;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: WebsitePublisherTransformer
 */

import com.ap.listing.model.WebsitePublisher;
import com.ap.listing.payload.response.WebsitePublisherResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.ArrayList;

@Component
@Slf4j
public class WebsitePublisherToResponseTransformer {

    public WebsitePublisherResponse transform(WebsitePublisher publisher) {
        if (publisher == null) return null;

        WebsitePublisherResponse response = new WebsitePublisherResponse();

        response.setWebsitePublisherId(publisher.getWebsitePublisherId());
        response.setPublishingId(publisher.getPublishingId());
        response.setWebsitePublishingStatus(publisher.getWebsitePublishingStatus());
        response.setDomain(publisher.getDomain());
        response.setLanguage(publisher.getLanguage());
        response.setSpecialRequirements(publisher.getSpecialRequirements());
        response.setProductSpecialRequirements(publisher.getProductSpecialRequirements());
        response.setCrowdPlacedContent(publisher.isCrowdPlacedContent());
        response.setSponsoredContent(publisher.isSponsoredContent());
        response.setCategories(new ArrayList<>(publisher.getCategories()));
        response.setBasicContentSize(publisher.getBasicContentSize());
        response.setLinkAttribute(publisher.getLinkAttribute());
        response.setContentPlacementPrice(publisher.getContentPlacementPrice());
        response.setWritingAndPlacementPrice(publisher.getWritingAndPlacementPrice());
        response.setExtraSizeContentWriting(publisher.getExtraSizeContentWriting());
        response.setSpecialTopicPricing(publisher.getSpecialTopicPricing());
        response.setExtraLinkPricing(publisher.getExtraLinkPricing());
        response.setLinksToBePlacedInOneArticle(publisher.getLinksToBePlacedInOneArticle());
        response.setLinkInsertionPrice(publisher.getLinkInsertionPrice());
        response.setLinkInsertionSpecialTopicPrice(publisher.getLinkInsertionSpecialTopicPrice());
        response.setBestArticleLinkForGuestPosting(
                publisher.getBestArticleLinkForGuestPosting() != null
                        ? new ArrayList<>(publisher.getBestArticleLinkForGuestPosting())
                        : null
        );
        response.setBestArticleLinkForLinkInsertion(
                publisher.getBestArticleLinkForLinkInsertion() != null
                        ? new ArrayList<>(publisher.getBestArticleLinkForLinkInsertion())
                        : null
        );
        response.setConsiderPlacingBuyerArticleForFree(publisher.isConsiderPlacingBuyerArticleForFree());
        response.setRequirementForSubmittingFreeArticle(publisher.getRequirementForSubmittingFreeArticle());
        response.setTat(publisher.getTat());
        response.setOtherLanguageSupported(publisher.getOtherLanguageSupported());
        response.setOwnershipType(publisher.getOwnershipType());
        response.setUserId(publisher.getUserId());
        response.setWebsite(publisher.getWebsite());
        response.setDateCreated(publisher.getDateCreated());
        response.setDateUpdated(publisher.getDateUpdated());
        log.info("Website publisher transformed response: {}", response);
        return response;
    }
}
