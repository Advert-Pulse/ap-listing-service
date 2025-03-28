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
    CATEGORY_MANDATORY("AP__ERROR-3005", "Category is mandatory and cannot be null" ),
    CATEGORY_LIMIT_FOUR("AP__ERROR-3006", "Website should not have more than four categories" ),
    CATEGORIES_INVALID("AP__ERROR-3007", "Some categories are invalid" ),
    SPECIAL_REQUIREMENT_LENGTH_EXCEED("AP__ERROR-3008", "Special requirement length should be less than 7000 characters" ),
    SPECIAL_REQUIREMENT_WORD_EXCEED("AP__ERROR-3009", "Special requirement should be less than 400 words" ),
    BASIC_CONTENT_SIZE_MANDATORY("AP__ERROR-3010", "Basic content size is mandatory and cannot be null" ),
    INVALID_BASIC_CONTENT_SIZE("AP__ERROR-3011", "Basic content size should be one of 500, 1000, 1500, 2000+" ),
    LINK_ATTRIBUTE_MANDATORY("AP__ERROR-3012", "Link attribute is mandatory and cannot be null" ),
    LINK_ATTRIBUTE_INVALID("AP__ERROR-3013", "Link attribute should be one of Dofollow or Nofollow" ),
    PRICING_INVALID("AP__ERROR-3014", "Pricing invalid" ),
    INVALID_LIST_URL("AP__ERROR-3015", "Invalid list URL" ),
    URL_LIST_SIZE_LENGTH_EXCEED("AP__ERROR-3016", "URL list size should be less 3" ),
    PRODUCT_SPECIAL_REQUIREMENT_LENGTH_EXCEED("AP__ERROR-3017", "Product special requirement length should be less than 7000 characters"),
    PRODUCT_SPECIAL_REQUIREMENT_WORD_EXCEED("AP__ERROR-3018", "Product special requirement should be less than 400 words"),
    FEIGN_RESOURCE_NOT_FOUND("AP__ERROR--3019", "Resource not found" ),
    FEIGN_BAD_REQUEST_EXCEPTION("AP__ERROR--3020", "Bad request" ),
    FEIGN_UNAUTHORIZED("AP__ERROR--3021", "Unauthorized" ),
    FEIGN_FORBIDDEN("AP__ERROR--3022", "Forbidden" ),
    FEIGN_INTERNAL_SERVER_ERROR("AP__ERROR--3023", "Internal server error" ),
    WEBSITE_ID_INVALID("AP__ERROR--3024", "Invalid website id" ),
    WEBSITE_NOT_FOUND_BY_ID("AP__ERROR--3025", "Website not found by given Id"),
    WEBSITE_ALREADY_ADDED("AP__ERROR--3026", "Website already added"),;

    private final String code;
    private final String message;

    ErrorData(String code, String message) {
        this.code = code;
        this.message = message;
    }
}
