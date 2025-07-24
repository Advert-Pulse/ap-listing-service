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
  File: OrderArticle
 */

import com.ap.listing.annotation.GeneratedCustomId;
import com.ap.listing.constants.EntityConstants;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import jakarta.persistence.*;
import lombok.*;

import java.util.Date;

@Entity
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@Table(
        name = EntityConstants.ORDER_ARTICLE,
        schema = EntityConstants.LISTING_SCHEMA
)
public class OrderArticle {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private String orderArticleId;

    @GeneratedCustomId(
            prefix = "APOD",
            sequence = "listing.ap_order_sequence",
            length = 7
    )
    @Column(unique = true)
    private String orderId;
    private String wordCount;
    private String category;

    @Column(length = 4000)
    private String titleSuggestion;

    @Column(length = 4000)
    private String keywords;

    @Column(length = 4000)
    private String contentGoal;

    @Column(length = 4000)
    private String targetAudience;

    @Column(length = 4000)
    private String sampleContent;

    @Column(length = 4000)
    private Integer articlePrice;

    @Lob
    @Column(length = 70000)
    private String briefRequirements;


    @Temporal(TemporalType.TIMESTAMP)
    private Date dateCreated;

    @Temporal(TemporalType.TIMESTAMP)
    private Date dateUpdated;

    @Column(nullable = false)
    private String userId;
}
