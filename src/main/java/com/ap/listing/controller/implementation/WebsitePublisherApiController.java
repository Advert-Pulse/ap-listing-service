package com.ap.listing.controller.implementation;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: WebsitePublisherApiController
 */

import com.ap.listing.constants.ApiConstants;
import com.ap.listing.controller.WebsitePublisherApi;
import com.ap.listing.payload.request.PublishWebsiteRequest;
import com.ap.listing.service.WebsitePublisherService;
import com.bloggios.provider.payload.ModuleResponse;
import com.bloggios.provider.utils.ControllerHelper;
import lombok.RequiredArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequiredArgsConstructor
public class WebsitePublisherApiController implements WebsitePublisherApi {

    private static final Logger LOGGER = LoggerFactory.getLogger(WebsitePublisherApiController.class);

    private final WebsitePublisherService websitePublisherService;

    @Override
    public ResponseEntity<ModuleResponse> publishSite(PublishWebsiteRequest publishWebsiteRequest, String websiteId) {
        return ControllerHelper.loggedResponse(
                () -> websitePublisherService.publishSite(publishWebsiteRequest, websiteId),
                ApiConstants.PUBLISH_SITE,
                LOGGER
        );
    }
}
