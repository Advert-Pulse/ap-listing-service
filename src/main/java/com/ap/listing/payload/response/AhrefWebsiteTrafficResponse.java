package com.ap.listing.payload.response;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: AhrefWebsiteTrafficResponse
 */

import com.ap.listing.payload.AhrefTopCountry;
import com.ap.listing.payload.AhrefTrafficHistory;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@ToString
public class AhrefWebsiteTrafficResponse {

    private String status;
    private String url;
    private long trafficMonthlyAvg;
    private long costMonthlyAvg;

    @JsonProperty("traffic_history")
    private List<AhrefTrafficHistory> trafficHistory;

    @JsonProperty("top_countries")
    private List<AhrefTopCountry> topCountries;

}
