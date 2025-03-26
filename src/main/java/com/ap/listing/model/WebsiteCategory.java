package com.ap.listing.model;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: WebsiteCategories
 */

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
@ToString
@Table(
        name = "website_categories",
        schema = "listing"
)
public class WebsiteCategory {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private String websiteCategoryId;

    @Column(nullable = false, unique = true)
    private String category;

    @Column(length = 1000)
    private String description;

    @Temporal(TemporalType.TIMESTAMP)
    private Date createdAt;

    @Temporal(TemporalType.TIMESTAMP)
    private Date updatedAt;

    @Column(
            nullable = false
    )
    private String createdBy;

    @Column(
            nullable = false
    )
    private String updatedBy;
}
