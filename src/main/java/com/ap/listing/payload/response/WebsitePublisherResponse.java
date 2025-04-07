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
package com.ap.listing.payload.response;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: PublishWebsiteResponse
 */

import com.ap.listing.model.WebsiteCategory;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.*;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@ToString
public class WebsitePublisherResponse {

    private String websitePublisherId;
    private String publishingId;
    private String websitePublishingStatus;
    private String domain;
    private String language;
    private String specialRequirements;
    private String productSpecialRequirements;
    private boolean crowdPlacedContent;
    private boolean isSponsoredContent;
    private List<WebsiteCategory> categories = new ArrayList<>();
    private String basicContentSize;
    private String linkAttribute;
    private Double contentPlacementPrice;
    private Double writingAndPlacementPrice;
    private Double extraSizeContentWriting;
    private Double specialTopicPricing;
    private Double extraLinkPricing;
    private String linksToBePlacedInOneArticle;
    private Double linkInsertionPrice;
    private Double linkInsertionSpecialTopicPrice;
    private List<String> bestArticleLinkForGuestPosting;
    private List<String> bestArticleLinkForLinkInsertion;
    private boolean considerPlacingBuyerArticleForFree;
    private String requirementForSubmittingFreeArticle;
    private Integer tat;
    private boolean otherLanguageSupported;
    private String ownershipType;
    private String userId;
    private WebsiteResponse website;
    private Date dateCreated;
    private Date dateUpdated;
}
