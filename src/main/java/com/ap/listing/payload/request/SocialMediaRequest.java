package com.ap.listing.payload.request;

/*
  Developer: Sudhanshu Nautiyal
  Project: ap-listing-service
  File: SocialMediaRequest
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
public class SocialMediaRequest {

    private String url;
    private String associatedSite;
    private Double price;
}
