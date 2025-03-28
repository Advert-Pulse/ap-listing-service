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
    private double contentPlacementPrice;
    private double writingAndPlacementPrice;
    private double extraSizeContentWriting;
    private double specialTopicPricing;
    private double extraLinkPricing;
    private String linksToBePlacedInOneArticle;

    // link insertion
    private double linkInsertionPrice;
    private double linkInsertionSpecialTopicPrice;

    @JdbcTypeCode(SqlTypes.ARRAY)
    @Column(columnDefinition = "text[]")
    private List<String> bestArticleLinkForGuestPosting;

    @JdbcTypeCode(SqlTypes.ARRAY)
    @Column(columnDefinition = "text[]")
    private List<String> bestArticleLinkForLinkInsertion;

    private boolean considerPlacingBuyerArticleForFree;
    private String requirementForSubmittingFreeArticle;

    private String tat;
    private String otherLanguageSupported;

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
