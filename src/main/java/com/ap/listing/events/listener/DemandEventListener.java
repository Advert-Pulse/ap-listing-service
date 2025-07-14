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
