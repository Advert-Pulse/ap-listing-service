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
  File: Website
 */

import com.ap.listing.constants.EntityConstants;
import com.ap.listing.payload.TopCountry;
import com.ap.listing.payload.TrafficHistory;
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
        name = EntityConstants.WEBSITE,
        schema = EntityConstants.LISTING_SCHEMA,
        indexes = {
                @Index(
                        name = "index_website_website",
                        columnList = "websiteId"
                )
        }
)
public class Website {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private String websiteId;

    @Column(unique = true, nullable = false)
    private String domain;

    @Temporal(TemporalType.TIMESTAMP)
    private Date dateCreated;

    @Temporal(TemporalType.TIMESTAMP)
    private Date dateUpdated;

    @Column(nullable = false)
    private String userId;

    private Boolean isAvailable;

    @OneToMany(mappedBy = "website", cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.EAGER)
    private List<WebsitePublisher> publishers = new ArrayList<>();

    private Double minPrice;
    private Double maxPrice;

    private boolean isDoFollow;
    private boolean isNoFollow;

    private String basicContentSize;
    private boolean isOwnerAvailable;
    private Integer tat;
    private boolean isExampleOfWork;
    private boolean isSponsoredContent;
    private boolean isContentPlacement;
    private boolean isWritingPlacement;

    @ManyToMany(fetch = FetchType.EAGER)
    @JoinTable(
            schema = "listing",
            joinColumns = @JoinColumn(name = "Website", referencedColumnName = "websiteId"),
            inverseJoinColumns = @JoinColumn(name = "WebsiteCategory", referencedColumnName = "websiteCategoryId")
    )
    @Builder.Default
    private List<WebsiteCategory> categories = new ArrayList<>();

    private String mozDa;
    private Integer majesticTf;
    private long ahrefOrganicTraffic;
    private long similarWebTraffic;
    private Integer domainRating;
    private Integer urlRating;

    private String countryCode;
    private String country;

    @JdbcTypeCode(SqlTypes.JSON)
    @Column(columnDefinition = "jsonb")
    private List<TrafficHistory> ahrefTrafficHistory;

    @JdbcTypeCode(SqlTypes.JSON)
    @Column(columnDefinition = "jsonb")
    private List<TopCountry> ahrefTopCountries;

    @JdbcTypeCode(SqlTypes.JSON)
    @Column(columnDefinition = "jsonb")
    private List<TrafficHistory> similarWebTrafficHistory;

    @JdbcTypeCode(SqlTypes.JSON)
    @Column(columnDefinition = "jsonb")
    private List<TopCountry> similarWebTopCountries;
}
