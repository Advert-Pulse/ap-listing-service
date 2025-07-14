package com.ap.listing.events;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: DemandEvent
 */

import com.ap.listing.payload.records.DemandDataRecord;
import lombok.Getter;
import org.springframework.context.ApplicationEvent;

@Getter
public class DemandEvent extends ApplicationEvent {

    private final DemandDataRecord demandDataRecord;

    public DemandEvent(DemandDataRecord demandDataRecord) {
        super(demandDataRecord);
        this.demandDataRecord = demandDataRecord;
    }
}
