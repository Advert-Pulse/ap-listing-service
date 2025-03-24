package com.ap.listing.model;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: Website
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

    private String domain;

    private String domainMetricsId;

    @Temporal(TemporalType.TIMESTAMP)
    private Date dateCreated;

    @Column(nullable = false)
    private String userId;
}
