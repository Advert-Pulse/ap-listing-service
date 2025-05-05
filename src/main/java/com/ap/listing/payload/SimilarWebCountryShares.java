package com.ap.listing.payload;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: SimilarWebCountryShares
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
public class SimilarWebCountryShares {

    private String country;
    private String countryCode;
    private String valueinPercentage;
    private String countryFlag;
    private String value;
}
