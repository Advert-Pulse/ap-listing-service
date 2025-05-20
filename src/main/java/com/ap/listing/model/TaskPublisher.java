package com.ap.listing.model;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: Task
 */

import com.ap.listing.annotation.GeneratedCustomId;
import com.ap.listing.payload.PricingPayload;
import com.ap.listing.payload.TaskStatusPayload;
import com.ap.listing.payload.UrlAnchorTextPayload;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.JdbcTypeCode;
import org.hibernate.type.SqlTypes;

import java.util.Date;
import java.util.List;

@Entity
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@ToString
public class TaskPublisher {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private String taskBuyerUUID;

    @GeneratedCustomId(
            prefix = "APTK",
            sequence = "listing.ap_task_sequence",
            length = 7
    )
    @Column(unique = true)
    private String taskBuyerId;

    // From Product Type Enum
    private String productType;

    private double totalPrice;

    @JdbcTypeCode(SqlTypes.JSON)
    @Column(columnDefinition = "jsonb")
    private List<PricingPayload> priceBreak;

    @JdbcTypeCode(SqlTypes.JSON)
    @Column(columnDefinition = "jsonb")
    private List<TaskStatusPayload> taskStatus;
    private String isSpecialTopic;

    @Temporal(TemporalType.TIMESTAMP)
    private Date dateCreated;

    @Lob
    @Column(length = 10000000)
    private String content;

    @Column(length = 70000)
    private String specialRequirements;

    @JdbcTypeCode(SqlTypes.JSON)
    @Column(columnDefinition = "jsonb")
    private List<UrlAnchorTextPayload> urlAnchorTexts;

    private String taskPlacementUrl;

    private String contentType;

    // publisher task status enum
    private String currentStatus;
}
