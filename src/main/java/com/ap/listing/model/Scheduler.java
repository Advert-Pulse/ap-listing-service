/**
 * Proprietary License Agreement
 * <p>
 * Copyright (c) 2025 Advert Pulse
 * <p>
 * This software is the confidential and proprietary property of Advert Pulse
 * and is licensed, not sold. The application owner is Advert Pulse, and the
 * developer and maintainer is Bloggios. Only authorized Bloggios administrators
 * are permitted to copy, modify, distribute, or sublicense this software under
 * the terms set forth in this agreement.
 * <p>
 * You may not:
 * 1. Copy, modify, distribute, or sublicense this software without express
 *    written permission from Advert Pulse or Bloggios.
 * 2. Reverse engineer, decompile, disassemble, or otherwise attempt to derive
 *    the source code of the software.
 * 3. Modify this license in any way, including but not limited to altering its
 *    terms, even by Advert Pulse or any other entity, without express written
 *    permission from Bloggios administrators. Bloggios is the creator of this
 *    license and retains exclusive rights to update or modify it.
 * 4. Update or modify the license without written permission from Bloggios
 *    administrators.
 * <p>
 * The software is provided "as is," and Advert Pulse makes no warranties,
 * express or implied, regarding the software, including but not limited to any
 * warranties of merchantability, fitness for a particular purpose, or
 * non-infringement.
 * <p>
 * For inquiries regarding licensing, please contact support@bloggios.com.
 */
package com.ap.listing.model;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: Scheduler
 */

import com.ap.listing.constants.EntityConstants;
import com.ap.listing.enums.ScheduleTaskType;
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
        name = EntityConstants.SCHEDULER,
        schema = EntityConstants.LISTING_SCHEMA
)
public class Scheduler {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    private String schedulerId;

    @Column(nullable = false)
    private String primaryId;

    @Column(columnDefinition = "boolean default false")
    private Boolean isSchedulingDone;

    @Enumerated(EnumType.STRING)
    private ScheduleTaskType scheduledTaskType;

    @Temporal(TemporalType.TIMESTAMP)
    private Date scheduleCompletedOn;

    @Temporal(TemporalType.TIMESTAMP)
    private Date createdOn;

    @Temporal(TemporalType.TIMESTAMP)
    private Date updatedOn;

    private int timesUsed;

    @Temporal(TemporalType.TIMESTAMP)
    private Date scheduledOn;

    private Boolean isPassed;

    @Column(length = 7000)
    private String message;
}
