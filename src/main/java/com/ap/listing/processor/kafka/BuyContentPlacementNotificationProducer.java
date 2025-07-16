package com.ap.listing.processor.kafka;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: BuyContentPlacementNotificationProducer
 */

import com.ap.listing.constants.EnvironmentConstants;
import com.ap.listing.kafka.producer.MessageProducer;
import com.ap.listing.payload.kafka.BuyContentPlacementEvent;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
@Slf4j
public class BuyContentPlacementNotificationProducer extends MessageProducer<BuyContentPlacementEvent> {

    private final Environment environment;

    @Override
    public String setTopic() {
        return environment.getProperty(EnvironmentConstants.BUY_CONTENT_PLACEMENT_TOPIC);
    }
}
