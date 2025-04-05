package com.ap.listing.controller.implementation;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: WebsiteCategoryApiController
 */

import com.ap.listing.constants.ApiConstants;
import com.ap.listing.controller.WebsiteCategoryApi;
import com.ap.listing.payload.request.WebsiteCategoryRequest;
import com.ap.listing.payload.response.ListResponse;
import com.ap.listing.payload.response.WebsiteCategoryResponse;
import com.ap.listing.service.WebsiteCategoryService;
import com.bloggios.provider.utils.ControllerHelper;
import lombok.RequiredArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequiredArgsConstructor
public class WebsiteCategoryApiController implements WebsiteCategoryApi {

    private static final Logger LOGGER = LoggerFactory.getLogger(WebsiteCategoryApiController.class);
    private final WebsiteCategoryService websiteCategoryService;

    @Override
    public ResponseEntity<WebsiteCategoryResponse> addWebsiteCategory(WebsiteCategoryRequest websiteCategoryRequest) {
        return ControllerHelper.loggedResponse(
                () -> websiteCategoryService.addWebsiteCategory(websiteCategoryRequest),
                ApiConstants.ADD_WEBSITE_CATEGORY,
                LOGGER
        );
    }

    @Override
    public ResponseEntity<ListResponse> getWebsiteCategoryList() {
        return ControllerHelper.loggedResponse(
                websiteCategoryService::getWebsiteCategoryList,
                ApiConstants.GET_WEBSITE_CATEGORY_LIST,
                LOGGER
        );
    }
}
