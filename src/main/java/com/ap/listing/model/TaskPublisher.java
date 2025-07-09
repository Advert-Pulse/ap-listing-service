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
  File: Task
 */

import com.ap.listing.annotation.CustomIdGeneratorListener;
import com.ap.listing.annotation.GeneratedCustomId;
import com.ap.listing.constants.EntityConstants;
import com.ap.listing.payload.PricingPayload;
import com.ap.listing.payload.PublisherTaskStatusPayload;
import com.ap.listing.payload.UrlAnchorTextPayload;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.JdbcTypeCode;
import org.hibernate.type.SqlTypes;

import java.util.Date;
import java.util.List;

@Entity
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@ToString
@Table(
        name = EntityConstants.TASK_PUBLISHER,
        schema = EntityConstants.LISTING_SCHEMA
)
@EntityListeners(CustomIdGeneratorListener.class)
public class TaskPublisher {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private String taskPublisherUUID;

    @GeneratedCustomId(
            prefix = "APTK",
            sequence = "listing.ap_task_sequence",
            length = 7
    )
    @Column(unique = true)
    private String taskId;

    // From Product Type Enum
    private String productType;

    private String siteUrl;

    @JdbcTypeCode(SqlTypes.JSON)
    @Column(columnDefinition = "jsonb")
    private List<PricingPayload> priceBreak;
    private double totalPrice;

    // publisher task status enum
    private String currentStatus;

    @JdbcTypeCode(SqlTypes.JSON)
    @Column(columnDefinition = "jsonb")
    private List<PublisherTaskStatusPayload> taskStatus;
    private String isSpecialTopic;

    @Temporal(TemporalType.TIMESTAMP)
    private Date dateCreated;

    @Temporal(TemporalType.TIMESTAMP)
    private Date dateUpdated;

    @Lob
    @Column(length = 10000000)
    private String content;

    @Column(length = 70000)
    private String specialRequirements;

    @JdbcTypeCode(SqlTypes.JSON)
    @Column(columnDefinition = "jsonb")
    private List<UrlAnchorTextPayload> urlAnchorTexts;

    private String taskPlacementUrl;

    private String contentType;

    private String buyerId;
    private String publisherId;

    private double platformFee;

    private String publishingId;
}
