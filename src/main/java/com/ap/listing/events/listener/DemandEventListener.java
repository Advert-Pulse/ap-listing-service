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
package com.ap.listing.events.listener;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: DemandEventListener
 */

import com.ap.listing.constants.ServiceConstants;
import com.ap.listing.dao.repository.DemandRepository;
import com.ap.listing.events.DemandEvent;
import com.ap.listing.feign.IPAPIFeign;
import com.ap.listing.model.Demand;
import com.ap.listing.payload.records.DemandDataRecord;
import com.ap.listing.payload.response.IPResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

import java.util.Date;
import java.util.Objects;

@Component
@RequiredArgsConstructor
@Slf4j
public class DemandEventListener {

    private final IPAPIFeign iPAPIFeign;
    private final DemandRepository demandRepository;

    @EventListener
    public void onEvent(DemandEvent event) {
        log.info("Received demand event: {}", event);
        DemandDataRecord demandDataRecord = event.getDemandDataRecord();
        if (demandDataRecord.remoteAddress().equalsIgnoreCase(ServiceConstants.LOCAL_REMOTE)) {
            log.info("Returning as local System IP found for taskId: {}", demandDataRecord.taskId());
            return;
        }
        IPResponse iPResponse = iPAPIFeign.getDetails(demandDataRecord.remoteAddress());
        log.info("IP Response from Feign Client : {}", iPResponse);
        if (Objects.isNull(iPResponse.getCountryCode())) {
            log.info("Returning as no country code found in Feign Response for ip: {}, taskId: {}", demandDataRecord.remoteAddress(), demandDataRecord.taskId());
            return;
        }
        Demand demand = build(iPResponse, demandDataRecord.taskId());
        Demand demandResponse = demandRepository.save(demand);
        log.info("Demand saved to database successfully: {}", demandResponse);
    }

    public Demand build(IPResponse ipResponse, String taskId) {
        Demand demand = Demand
                .builder()
                .countryCode(ipResponse.getCountryCode())
                .countryName(ipResponse.getCountry())
                .region(ipResponse.getRegionName())
                .timeZone(ipResponse.getTimezone())
                .taskId(taskId)
                .demandDate(new Date())
                .build();
        log.info("Demand build: {}", demand);
        return demand;
    }
}
