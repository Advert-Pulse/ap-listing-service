package com.ap.listing.payload.response;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: DomainMetricsFeignResponse
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
public class DomainMetricsFeignResponse {

    private String domain;
    private String domainAsEntered;
    private String mozLinks;
    private String mozPA;
    private String mozDA;
    private String mozRank;
    private String mozTrust;
    private String mozSpam;
    private String majesticStatReturned;
    private String fbComments;
    private String fbShares;
    private String stumbles;
    private String pinterestPins;
    private String majesticLinks;
    private String majesticRefDomains;
    private String majesticRefEDU;
    private String majesticRefGov;
    private String majesticRefSubnets;
    private String majesticIPs;
    private String majesticCF;
    private String majesticTTF0Name;
    private String majesticTTF0Value;
    private String majesticTTF1Name;
    private String majesticTTF1Value;
    private String majesticTTF2Name;
    private String majesticTTF2Value;
    private String majesticTF;
}
