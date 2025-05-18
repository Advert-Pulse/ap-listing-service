package com.ap.listing.payload.request;

import com.ap.listing.enums.WebsitePublishingStatus;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.*;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: ManagePublisherRequest
 */

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@ToString
public class ManagePublisherRequest {

    private WebsitePublishingStatus status;
    private String message;
    private String websitePublisherId;

}
