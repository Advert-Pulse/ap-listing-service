package com.ap.listing.model;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: OrderArticle
 */

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
        schema = EntityConstants.LISTING_SCHEMA,
        indexes = {
                @Index(
                        name = "index_website_website",
                        columnList = "websiteId"
                )
        }
)
public class OrderArticle {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private String orderArticleId;
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
