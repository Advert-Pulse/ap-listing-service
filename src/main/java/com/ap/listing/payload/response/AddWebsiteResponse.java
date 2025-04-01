package com.ap.listing.payload.response;

import com.ap.listing.model.DomainMetrics;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.*;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: AddWebsiteResponse
 */

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@ToString
public class AddWebsiteResponse {

    private String websiteId;
    private String domain;
    private Boolean isAvailable;
    private String websitePublisherId;
    private boolean isNewDomain;
}
