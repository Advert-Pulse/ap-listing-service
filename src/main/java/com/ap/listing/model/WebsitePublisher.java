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

    private String websiteId;
    private String userId;
}
