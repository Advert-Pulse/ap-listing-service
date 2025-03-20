package com.ap.listing.model;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: Preference
 */

import com.ap.listing.constants.EntityConstants;
import com.bloggios.provider.payload.BaseModel;
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
        name = EntityConstants.PREFERENCE,
        schema = EntityConstants.LISTING_SCHEMA,
        indexes = {
                @Index(
                        name = "index_preference_userId",
                        columnList = "userId"
                )
        }
)
public class Preference extends BaseModel {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private String preferenceId;

    private String userId;

    private String preferenceType;

    @PrePersist
    protected void onCreate() {
        Date now = new Date();
        this.setCreatedAt(now);
        this.setUpdatedAt(now);
    }

    @PreUpdate
    protected void onUpdate() {
        this.setUpdatedAt(new Date());
    }
}
