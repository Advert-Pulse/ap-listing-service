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
    PREFERENCE_NOT_ADDED("AP__ERROR--3003", "Preference is not added yet" ),;

    private final String code;
    private final String message;

    ErrorData(String code, String message) {
        this.code = code;
        this.message = message;
    }
}
