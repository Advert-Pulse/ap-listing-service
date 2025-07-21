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
package com.ap.listing.service.implementation;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: DemandServiceImplementation
 */

import com.ap.listing.dao.repository.DemandRepository;
import com.ap.listing.enums.ErrorData;
import com.ap.listing.exception.BadRequestException;
import com.ap.listing.model.Demand;
import com.ap.listing.payload.response.DemandDataResponse;
import com.ap.listing.payload.response.ListResponse;
import com.ap.listing.service.DemandService;
import com.bloggios.provider.utils.DateUtils;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Slf4j
public class DemandServiceImplementation implements DemandService {

    private final DemandRepository demandRepository;

    @Override
    public ResponseEntity<ListResponse> getDemand(Integer days) {
        if (days <= 0 || days > 180) throw new BadRequestException(ErrorData.GET_DEMAND_DAYS_ERROR);
        Date now = new Date();
        Date date = DateUtils.addDays(now, -days);
        List<Demand> demandList = demandRepository.findByCountryCodeAndDemandDateInRange(date, now);
        Map<String, Long> countryDemandCount = demandList.stream()
                .collect(Collectors.groupingBy(Demand::getCountryCode, Collectors.counting()));
        long totalDemands = demandList.size();
        List<DemandDataResponse> responseList = countryDemandCount.entrySet().stream()
                .map(entry -> {
                    String country = entry.getKey();
                    int demand = entry.getValue().intValue();
                    int demandPercent = (int) Math.round((demand * 100.0) / totalDemands);
                    return new DemandDataResponse(country, demand, demandPercent);
                })
                .collect(Collectors.toList());
        return ResponseEntity.ok(
                ListResponse
                        .builder()
                        .size(responseList.size())
                        .totalRecordsCount(responseList.size())
                        .object(responseList)
                        .build()
        );
    }
}
