package com.ap.listing.model;

import com.ap.listing.constants.EntityConstants;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import jakarta.persistence.*;
import lombok.*;

/*
  Developer: Sudhanshu Nautiyal
  Project: ap-listing-service
  File: SocialMedia
 */
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
@ToString
@Entity
@JsonIgnoreProperties(ignoreUnknown = true)
@Table(
        name = EntityConstants.SOCIAL_MEDIA,
        schema = EntityConstants.SOCIAL_MEDIA
)
public class SocialMedia {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private String socialId;

    @Column(unique = true, nullable = false)
    private String url;

    @Column(unique = true, nullable = false)
    private String associatedSite;

    @Column(nullable = false)
    private Double price;

}
