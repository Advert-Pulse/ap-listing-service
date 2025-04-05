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
  File: PublishWebsiteRequestToWebsitePublisherTransformer
 */

import com.ap.listing.dao.repository.WebsiteCategoryRepository;
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

    public WebsitePublisher transform(PublishWebsiteRequest publishWebsiteRequest, WebsitePublisher websitePublisher) {

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
        log.info("{} >> transform -> websitePublisher: {}", getClass().getSimpleName(), websitePublisher.toString());
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
