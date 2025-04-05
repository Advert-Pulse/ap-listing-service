package com.ap.listing.payload.response;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: PublishWebsiteResponse
 */

import com.ap.listing.model.Website;
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
    private double contentPlacementPrice;
    private double writingAndPlacementPrice;
    private double extraSizeContentWriting;
    private double specialTopicPricing;
    private double extraLinkPricing;
    private String linksToBePlacedInOneArticle;
    private double linkInsertionPrice;
    private double linkInsertionSpecialTopicPrice;
    private List<String> bestArticleLinkForGuestPosting;
    private List<String> bestArticleLinkForLinkInsertion;
    private boolean considerPlacingBuyerArticleForFree;
    private String requirementForSubmittingFreeArticle;
    private String tat;
    private String otherLanguageSupported;
    private String ownershipType;
    private String userId;
    private Website website;
    private Date dateCreated;
    private Date dateUpdated;
}
