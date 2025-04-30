package com.ap.listing.payload.request;

/*
  Developer: Sudhanshu Nautiyal
  Project: ap-listing-service
  File: AddMultipleWebsiteRequest
 */


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.*;

import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@ToString
public class AddMultipleWebsiteRequest {
    private List<String> websites;
}