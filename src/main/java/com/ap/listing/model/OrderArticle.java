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
    private String titleSuggestion;
    private String keywords;
    private String contentGoal;
    private String targetAudience;
    private String sampleContent;
    private Integer articlePrice;


    @Temporal(TemporalType.TIMESTAMP)
    private Date dateCreated;

    @Temporal(TemporalType.TIMESTAMP)
    private Date dateUpdated;

    @Column(nullable = false)
    private String userId;
}
