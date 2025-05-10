package com.ap.listing.enums;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: OrderArticlePublishingStatus
 */

public enum OrderArticlePublishingStatus {
    COPYWRITER_ACCEPTANCE("Copywriter Acceptance"),
    IN_PROGRESS("In Progress"),
    YOUR_APPROVAL("Your Approval"),
    IMPROVEMENT("Improvement"),
    COMPLETED("Completed"),
    REJECTED("Rejected");

    private final String value;

    OrderArticlePublishingStatus(String value) {
        this.value = value;
    }
}
