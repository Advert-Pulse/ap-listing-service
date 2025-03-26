package com.ap.listing.enums;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: WebsitePublishingStatus
 */

public enum WebsitePublishingStatus {

    PENDING_INDEXATION("Pending Indexation"),
    PENDING_SPECIFICATION("Pending Specification"),
    PENDING_MODERATION("Pending Moderation"),
    APPROVED("Approved"),
    REJECTED("Rejected"),
    ON_HOLD("On Hold"),
    DELETE("Delete");

    private final String value;

    WebsitePublishingStatus(String value) {
        this.value = value;
    }
}
