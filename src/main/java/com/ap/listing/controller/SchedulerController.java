package com.ap.listing.controller;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: SchedulerController
 */

import com.ap.listing.constants.ApiConstants;
import com.ap.listing.scheduler.service.FetchWebsiteDataScheduler;
import com.ap.listing.scheduler.service.WebsitePublisherScheduler;
import com.bloggios.provider.payload.ModuleResponse;
import com.bloggios.provider.utils.ControllerHelper;
import lombok.RequiredArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/v1/scheduler")
@RequiredArgsConstructor
public class SchedulerController {

    private static final Logger LOGGER = LoggerFactory.getLogger(SchedulerController.class);

    private final FetchWebsiteDataScheduler fetchWebsiteDataScheduler;
    private final WebsitePublisherScheduler websitePublisherScheduler;

    @GetMapping("/fetch-website")
    public ResponseEntity<ModuleResponse> executeFetchWebsiteScheduler() {
        return ControllerHelper.loggedResponse(
                ()-> {
                    fetchWebsiteDataScheduler.doProcess();
                    return ResponseEntity.ok(ModuleResponse.builder().message("Executed").build());
                },
                ApiConstants.EXECUTE_FETCH_WEBSITE_SCHEDULER,
                LOGGER
        );
    }

    @GetMapping("/approve-website")
    public ResponseEntity<ModuleResponse> executeApproveWebsiteScheduler() {
        return ControllerHelper.loggedResponse(
                ()-> {
                    websitePublisherScheduler.doProcess();
                    return ResponseEntity.ok(ModuleResponse.builder().message("Executed").build());
                },
                ApiConstants.EXECUTE_FETCH_WEBSITE_SCHEDULER,
                LOGGER
        );
    }
}
