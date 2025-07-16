package com.ap.listing.payload.kafka;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: BuyContentPlacementEvent
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
public class BuyContentPlacementEvent {

    private String buyerId;
    private String publisherId;
    private String taskId;
    private String publishingId;
    private String domain;
    private double price;
}
