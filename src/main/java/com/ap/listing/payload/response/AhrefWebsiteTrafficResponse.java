package com.ap.listing.payload.response;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: AhrefWebsiteTrafficResponse
 */

import com.ap.listing.payload.TopCountry;
import com.ap.listing.payload.TrafficHistory;
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
    private List<TrafficHistory> trafficHistory;

    @JsonProperty("top_countries")
    private List<TopCountry> topCountries;

}
