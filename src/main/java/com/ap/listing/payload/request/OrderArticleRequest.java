package com.ap.listing.payload.request;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: OrderArticleRequest
 */

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@ToString
public class OrderArticleRequest {

    private String wordCount;
    private String category;
    private String titleSuggestion;
    private String keywords;
    private String contentGoal;
    private String targetAudience;
    private String sampleContent;
    private Integer articlePrice;
}
