package com.ap.listing.payload;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: AhrefTrafficDataResponse
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
public class AhrefTrafficDataResponse {

    private long trafficMonthlyAvg;
    private long costMonthlyAvg;
}
