package com.ap.listing.enums;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: ErrorData
 */

import lombok.Getter;

@Getter
public enum ErrorData {

    INVALID_PREFERENCE("AP__ERROR--3001", "Preference should be one of Seller or Buyer"),
    INTERNAL_ERROR("AP__ERROR--3002", "Internal error occurred in ap-listing-service"),
    PREFERENCE_NOT_ADDED("AP__ERROR--3003", "Preference is not added yet" ), 
    WEBSITE_IRRESPONSIVE("AP__ERROR--3004", "This website is not available or cannot be added"),
    FEIGN_RESOURCE_NOT_FOUND("AP__ERROR--3005", "Resource not found" ),
    FEIGN_BAD_REQUEST_EXCEPTION("AP__ERROR--3006", "Bad request" ),
    FEIGN_UNAUTHORIZED("AP__ERROR--3007", "Unauthorized" ),
    FEIGN_FORBIDDEN("AP__ERROR--3008", "Forbidden" ),
    FEIGN_INTERNAL_SERVER_ERROR("AP__ERROR--3009", "Internal server error" ),;

    private final String code;
    private final String message;

    ErrorData(String code, String message) {
        this.code = code;
        this.message = message;
    }
}
