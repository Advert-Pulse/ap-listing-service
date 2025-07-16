package com.ap.listing.processor;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: BuyContentPlacementNotificationProcessor
 */

import com.ap.listing.model.TaskPublisher;
import com.ap.listing.payload.kafka.BuyContentPlacementEvent;
import com.ap.listing.processor.kafka.BuyContentPlacementNotificationProducer;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
@Slf4j
public class BuyContentPlacementNotificationProcessor {

    private final BuyContentPlacementNotificationProducer buyContentPlacementNotificationProducer;

    public void process(TaskPublisher taskPublisherResponse) {
        log.info("{} >> process -> taskPublisherResponse: {}", taskPublisherResponse.getClass().getSimpleName(), taskPublisherResponse);
        BuyContentPlacementEvent buyContentPlacementEvent = BuyContentPlacementEvent
                .builder()
                .buyerId(taskPublisherResponse.getBuyerId())
                .publisherId(taskPublisherResponse.getPublisherId())
                .taskId(taskPublisherResponse.getTaskId())
                .domain(taskPublisherResponse.getSiteUrl())
                .publishingId(taskPublisherResponse.getPublishingId())
                .build();
        buyContentPlacementNotificationProducer.sendMessage(buyContentPlacementEvent);
    }
}
