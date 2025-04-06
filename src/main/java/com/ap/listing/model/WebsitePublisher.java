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
  File: WebsitePublisher
 */

import com.ap.listing.constants.EntityConstants;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.JdbcTypeCode;
import org.hibernate.type.SqlTypes;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

@Entity
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@Table(
        name = EntityConstants.WEBSITE_PUBLISHER,
        schema = EntityConstants.LISTING_SCHEMA,
        indexes = {
                @Index(
                        name = "index_website_publisher_user_id",
                        columnList = "userId"
                )
        }
)
public class WebsitePublisher {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private String websitePublisherId;

    @Column(nullable = false, unique = true, updatable = false)
    private String publishingId;

    // Use WebsitePublishingStatus Enum values
    private String websitePublishingStatus;

    @Column(nullable = false)
    private String domain;

    private String language;

    @Lob
    @Column(length = 70000)
    private String specialRequirements;

    @Lob
    @Column(length = 70000)
    private String productSpecialRequirements;

    private boolean crowdPlacedContent;

    private boolean isSponsoredContent;

    @ManyToMany(fetch = FetchType.EAGER)
    @JoinTable(
            schema = "listing",
            joinColumns = @JoinColumn(name = "WebsitePublisher", referencedColumnName = "websitePublisherId"),
            inverseJoinColumns = @JoinColumn(name = "WebsiteCategory", referencedColumnName = "websiteCategoryId")
    )
    @Builder.Default
    private List<WebsiteCategory> categories = new ArrayList<>();

    //guest posting
    // 500, 1000, 1500, 2000+
    private String basicContentSize;

    // dofollow and nofollow
    private String linkAttribute;
    private Double contentPlacementPrice;
    private Double writingAndPlacementPrice;
    private Double extraSizeContentWriting;
    private Double specialTopicPricing;
    private Double extraLinkPricing;
    private String linksToBePlacedInOneArticle;

    // link insertion
    private Double linkInsertionPrice;
    private Double linkInsertionSpecialTopicPrice;

    @JdbcTypeCode(SqlTypes.ARRAY)
    @Column(columnDefinition = "text[]")
    private List<String> bestArticleLinkForGuestPosting;

    @JdbcTypeCode(SqlTypes.ARRAY)
    @Column(columnDefinition = "text[]")
    private List<String> bestArticleLinkForLinkInsertion;

    private boolean considerPlacingBuyerArticleForFree;
    private String requirementForSubmittingFreeArticle;

    private String tat;
    private boolean otherLanguageSupported;

    // OwnershipType Enum
    private String ownershipType;

    private String userId;

    @ManyToOne(fetch = FetchType.LAZY)
    private Website website;

    @Temporal(TemporalType.TIMESTAMP)
    private Date dateCreated;

    @Temporal(TemporalType.TIMESTAMP)
    private Date dateUpdated;
}
