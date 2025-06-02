package com.ap.listing.model;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: BuyerImprovement
 */

import com.ap.listing.enums.BuyerImprovementStatus;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import jakarta.persistence.*;
import lombok.*;

import java.util.Date;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@ToString
public class BuyerImprovement {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private String buyerImprovementId;

    private String taskId;
    private String message;

    private String publisherId;
    private String buyerId;

    @Enumerated(EnumType.STRING)
    private BuyerImprovementStatus buyerImprovementStatus;

    @Temporal(TemporalType.TIMESTAMP)
    private Date dateCreated;

    @Temporal(TemporalType.TIMESTAMP)
    private Date dateUpdated;
}
