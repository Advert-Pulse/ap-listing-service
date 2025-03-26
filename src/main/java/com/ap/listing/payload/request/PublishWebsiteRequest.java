package com.ap.listing.payload.request;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: PublishWebsiteRequest
 */

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.*;

import java.util.ArrayList;
import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@ToString
public class PublishWebsiteRequest {

    private String specialRequirements;
    private String specialRequirementsHtml;
    private String productSpecialRequirements;
    private String productSpecialRequirementsHtml;
    private boolean crowdPlacedContent;
    private boolean isSponsoredContent;
    private List<String> categories = new ArrayList<>();
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

    // added in the google sheet
    private boolean otherLanguageSupported;
    private String tat;
}
