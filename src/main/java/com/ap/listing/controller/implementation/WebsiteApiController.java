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
    public ResponseEntity<ModuleResponse> addMultipleWebsite(List<String> websites) {
        return ControllerHelper.loggedResponse(
                () -> websiteService.addMultipleWebsite(websites),
                ApiConstants.ADD_WEBSITE,
                LOGGER
        );
    }
}
