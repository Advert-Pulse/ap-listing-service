package com.ap.listing.payload.response;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: WebsiteResponse
 */

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.*;

import java.util.Date;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@ToString
public class WebsiteResponse {

    private String websiteId;
    private String domain;
    private Date dateCreated;
    private Date dateUpdated;
    private String userId;
    private Boolean isAvailable;

}
