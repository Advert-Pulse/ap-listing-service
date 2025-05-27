package com.ap.listing.controller;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: PublisherController
 */

import com.ap.listing.constants.ApiConstants;
import com.ap.listing.service.PublisherService;
import com.bloggios.provider.payload.ModuleResponse;
import com.bloggios.provider.utils.ControllerHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/publisher")
public class PublisherController {

    private static final Logger LOGGER = LoggerFactory.getLogger(PublisherController.class);
    private final PublisherService publisherService;

    public PublisherController(PublisherService publisherService) {
        this.publisherService = publisherService;
    }

    public ResponseEntity<ModuleResponse> manageTaskInitial(
            @PathVariable String taskId,
            @RequestParam String status
    ) {
        return ControllerHelper.loggedResponse(
                ()-> publisherService.manageTaskInitial(taskId, status),
                ApiConstants.MANAGE_TASK_INITIAL,
                LOGGER
        );
    }
}
