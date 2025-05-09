package com.ap.listing.payload.response;

/*
  Developer: Sudhanshu Nautiyal
  Project: ap-listing-service
  File: SocialMediaResponse
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
public class SocialMediaResponse {

    private String url;
    private String associatedSite;
    private Double price;
}
