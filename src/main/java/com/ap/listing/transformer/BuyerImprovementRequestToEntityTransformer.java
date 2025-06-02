package com.ap.listing.transformer;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: BuyerImprovementRequestToEntityTransformer
 */

import com.ap.listing.enums.BuyerImprovementStatus;
import com.ap.listing.model.BuyerImprovement;
import com.ap.listing.model.TaskBuyer;
import com.ap.listing.payload.request.BuyerImprovementRequest;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.Date;

@Component
@RequiredArgsConstructor
@Slf4j
public class BuyerImprovementRequestToEntityTransformer {

    public BuyerImprovement transform(BuyerImprovementRequest buyerImprovementRequest, TaskBuyer taskBuyer) {
        Date now = new Date();
        BuyerImprovement buyerImprovement = BuyerImprovement
                .builder()
                .taskId(buyerImprovementRequest.getTaskId())
                .message(buyerImprovementRequest.getMessage())
                .publisherId(taskBuyer.getPublisherId())
                .buyerId(taskBuyer.getBuyerId())
                .dateCreated(now)
                .dateUpdated(now)
                .buyerImprovementStatus(BuyerImprovementStatus.PENDING)
                .build();
        log.info("Buyer Improvement transformed: {}", buyerImprovement.toString());
        return buyerImprovement;
    }
}
