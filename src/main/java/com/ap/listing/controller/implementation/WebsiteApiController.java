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
package com.ap.listing.controller.implementation;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: WebsiteApiController
 */

import com.ap.listing.constants.ApiConstants;
import com.ap.listing.controller.WebsiteApi;
import com.ap.listing.payload.response.AddWebsiteResponse;
import com.ap.listing.service.WebsiteService;
import com.bloggios.provider.payload.ModuleResponse;
import com.bloggios.provider.utils.ControllerHelper;
import lombok.RequiredArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@RequiredArgsConstructor
public class WebsiteApiController implements WebsiteApi {

    private static final Logger LOGGER = LoggerFactory.getLogger(WebsiteApiController.class);

    private final WebsiteService websiteService;

    @Override
    public ResponseEntity<AddWebsiteResponse> addWebsite(String website) {
        return ControllerHelper.loggedResponse(
                () -> websiteService.addWebsite(website),
                ApiConstants.ADD_WEBSITE,
                LOGGER
        );
    }

    @Override
    public ResponseEntity<List<AddWebsiteResponse>> addMultipleWebsite(List<String> websites) {
        return ControllerHelper.loggedResponse(
                () -> websiteService.addMultipleWebsite(websites),
                ApiConstants.ADD_WEBSITE,
                LOGGER
        );
    }
}
