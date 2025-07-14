/**
 * Proprietary License Agreement
 * <p>
 * Copyright (c) 2025 Advert Pulse
 * <p>
 * This software is the confidential and proprietary property of Advert Pulse
 * and is licensed, not sold. The application owner is Advert Pulse, and the
 * developer and maintainer is Bloggios. Only authorized Bloggios administrators
 * are permitted to copy, modify, distribute, or sublicense this software under
 * the terms set forth in this agreement.
 * <p>
 * You may not:
 * 1. Copy, modify, distribute, or sublicense this software without express
 *    written permission from Advert Pulse or Bloggios.
 * 2. Reverse engineer, decompile, disassemble, or otherwise attempt to derive
 *    the source code of the software.
 * 3. Modify this license in any way, including but not limited to altering its
 *    terms, even by Advert Pulse or any other entity, without express written
 *    permission from Bloggios administrators. Bloggios is the creator of this
 *    license and retains exclusive rights to update or modify it.
 * 4. Update or modify the license without written permission from Bloggios
 *    administrators.
 * <p>
 * The software is provided "as is," and Advert Pulse makes no warranties,
 * express or implied, regarding the software, including but not limited to any
 * warranties of merchantability, fitness for a particular purpose, or
 * non-infringement.
 * <p>
 * For inquiries regarding licensing, please contact support@bloggios.com.
 */
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
    WEBSITE_PUBLISHER_ID_INVALID("AP__ERROR--3024", "Invalid website publisher id" ),
    WEBSITE_NOT_FOUND_BY_ID("AP__ERROR--3025", "Website not found by given Id"),
    WEBSITE_ALREADY_ADDED("AP__ERROR--3026", "Website already added"),
    WEBSITE_PUBLISHER_NOT_FOUND("AP__ERROR--3027", "Website publisher not found" ),
    WEBSITE_PUBLISHER_NOT_FOUND_PUBLISHING_ID("AP__ERROR--3028", "Website publisher not found using publishing Id" ),
    ACCESS_DENIED_GET_PUBLISH_WEBSITE("AP__ERROR--3029", "Not authorized to get the details of a Website Publisher" ),
    WEBSITE_CATEGORY_ALREADY_PRESENT("AP__ERROR--3301", "Website category already exists" ),
    MY_PUBLISHED_SITE_USER_ID_FILTER("AP__ERROR--3302", "User Id is not allowed in Filter for my published site" ),
    NO_LIST_ADDED("AP__ERROR--3303", "Please add a list of websites" ),
    ONLY_ONE_WEBSITE_ADDED("AP__ERROR--3304", "Only one website added" ),
    WEBSITE_MAX_SIZE("AP__ERROR--3305", "Maximum number of websites allowed is 5" ),
    SOCIAL_MEDIA_ID_INVALID("AP__ERROR--3306", "Invalid social media id" ),
    SOCIAL_MEDIA_NOT_FOUND("AP__ERROR--3307", "Social media not found by Id" ),
    INVALID_WEBSITE_PUBLISHER_ID("AP__ERROR--3308", "Invalid website publisher id" ),
    WEBSITE_PUBLISHER_CANNOT_BE_DELETED("AP__ERROR--3309", "Website publisher cannot be deleted. Please use delete API to delete the data" ),
    WEBSITE_PUBLISHING_STATUS_NOT_MODERATION("AP__ERROR--3310", "Website publishing status should be in Pending Moderation or On Hold Status" ), 
    ONE_URL_ANCHOR_TEXT_NEEDED("AP__ERROR--3311", "Atleast one URL with Anchor Text is needed"), 
    ONLY_THREE_URL_ANCHOR_TEXT_ALLOWED("AP__ERROR--3312", "Only three URL with Anchor Text is allowed" ),
    URL_MANDATORY_FOR_URL_ANCHOR_TEXT("AP__ERROR--3313", "Url mandatory for URL with Anchor text" ),
    ANCHOR_TEXT_MANDATORY_FOR_URL_ANCHOR_TEXT("AP__ERROR--3314", "Anchor Text mandatory for URL with Anchor text" ),
    NO_EXTRA_LINKS_ALLOWED("AP__ERROR--3315", "No extra links allowed" ),
    TOKEN_NOT_PRESENT("AP__ERROR--3316", "Token not present in the request Header" ),
    INSUFFICIENT_FUNDS("AP__ERROR--3317", "Insufficient Funds" ),
    WEBSITE_PUBLISHER_NOT_APPROVED("AP__ERROR--3318", "Website publisher not approved" ),
    TASK_BUYER_NOT_FOUND("AP__ERROR--3319", "Task buyer not found" ),
    TASK_PUBLISHER_NOT_FOUND("AP__ERROR--3320", "Task publisher not found" ),
    MANAGE_TASK_INITIAL_STATUS_INVALID("AP__ERROR--3321", "Manage Initial Status invalid" ),
    CANNOT_MANAGE_OTHERS_TASK("AP__ERROR--3322", "Cannot Manage other Tasks" ),
    TASK_SHOULD_BE_IN_YOUR_ACCEPTANCE_TO_MANAGE_IT_INITIALLY("AP__ERROR--3323", "Task should be in Your Acceptance To Manage it Initially" ),
    TASK_SHOULD_BE_IN_IN_PROGRESS_OR_IMPROVEMENT("AP__ERROR--3324", "Task should be in In Progress or Improvement for initial approval" ),
    TASK_SHOULD_BE_BUYER_APPROVAL_TO_MANAGE("AP__ERROR--3325", "Task should be in Your (Buyer) Approval To Manage" ),
    WEBSITE_ID_NOT_VALID("AP__ERROR--3326", "Website id is not valid" ),
    GOOGLE_AUTH_TOKEN_EXCHANGE_ERROR("AP__ERROR--3327", "Google auth token exchange error" ),
    GET_DEMAND_DAYS_ERROR("AP__ERROR--3328", "Demand days should be greater than 0 and less than 180" ),;

    private final String code;
    private final String message;

    ErrorData(String code, String message) {
        this.code = code;
        this.message = message;
    }
}
