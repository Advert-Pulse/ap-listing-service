/**
 * Proprietary License Agreement
 * <p>
 * Copyright (c) 2025 Advert Pulse
 * <p>
 * This software is the confidential and proprietary property of Advert Pulse
 * and is licensed, not sold. The application owner is Advert Pulse, and the
 * developer and maintainer is Bloggios. Only authorized Bloggios administrators
 * are permitted to copy, modify, distribute, or sublicense this software under
 * the terms set forth in this agreement.
 * <p>
 * You may not:
 * 1. Copy, modify, distribute, or sublicense this software without express
 *    written permission from Advert Pulse or Bloggios.
 * 2. Reverse engineer, decompile, disassemble, or otherwise attempt to derive
 *    the source code of the software.
 * 3. Modify this license in any way, including but not limited to altering its
 *    terms, even by Advert Pulse or any other entity, without express written
 *    permission from Bloggios administrators. Bloggios is the creator of this
 *    license and retains exclusive rights to update or modify it.
 * 4. Update or modify the license without written permission from Bloggios
 *    administrators.
 * <p>
 * The software is provided "as is," and Advert Pulse makes no warranties,
 * express or implied, regarding the software, including but not limited to any
 * warranties of merchantability, fitness for a particular purpose, or
 * non-infringement.
 * <p>
 * For inquiries regarding licensing, please contact support@bloggios.com.
 */
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

    @OneToOne(cascade = CascadeType.ALL, fetch = FetchType.EAGER)
    private Website website;
}
