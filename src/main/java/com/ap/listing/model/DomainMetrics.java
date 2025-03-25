package com.ap.listing.model;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: DomainMetrics
 */

import com.ap.listing.constants.EntityConstants;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import jakarta.persistence.*;
import lombok.*;

@Entity
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@Table(
        name = EntityConstants.DOMAIN_METRICS,
        schema = EntityConstants.LISTING_SCHEMA,
        indexes = {
                @Index(
                        name = "index_domain_metrics_website",
                        columnList = "domainMetricsId"
                )
        }
)
public class DomainMetrics {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private String domainMetricsId;

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

    @OneToOne(mappedBy = "domainMetrics")
    private Website website;
}
