package com.ap.listing.model;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: Demand
 */

import com.ap.listing.constants.EntityConstants;
import jakarta.persistence.*;
import lombok.*;

import java.util.Date;

@Entity
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@ToString
@Table(
        name = EntityConstants.DEMAND_TABLE,
        schema = EntityConstants.LISTING_SCHEMA
)
public class Demand {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private String demandId;

    private String countryName;
    private String countryCode;
    private String taskId;
    private String remoteAddress;

    @Temporal(TemporalType.TIMESTAMP)
    private Date demandDate;

}
