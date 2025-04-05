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

    private final WebsiteToWebsiteResponseTransformer websiteToWebsiteResponseTransformer;

    public WebsitePublisherToResponseTransformer(WebsiteToWebsiteResponseTransformer websiteToWebsiteResponseTransformer) {
        this.websiteToWebsiteResponseTransformer = websiteToWebsiteResponseTransformer;
    }

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
        response.setWebsite(websiteToWebsiteResponseTransformer.transform(publisher.getWebsite()));
        response.setDateCreated(publisher.getDateCreated());
        response.setDateUpdated(publisher.getDateUpdated());
        log.info("Website publisher transformed response: {}", response.toString());
        return response;
    }
}
