package com.ap.listing.payload;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: SimilarWebTrafficHistory
 */

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.util.List;
import java.util.Map;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@ToString
public class SimilarWebTrafficHistory {

    @JsonProperty("Version")
    private int version;

    @JsonProperty("SiteName")
    private String siteName;

    @JsonProperty("Description")
    private String description;

    @JsonProperty("Title")
    private String title;

    @JsonProperty("Engagments")
    private SimilarWebEngagement engagements;

    @JsonProperty("EstimatedMonthlyVisits")
    private Map<String, Long> estimatedMonthlyVisits;

    @JsonProperty("TopCountryShares")
    private List<SimilarWebCountryShares> topCountryShares;
}
