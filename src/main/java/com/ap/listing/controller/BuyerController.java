package com.ap.listing.controller;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: BuyerController
 */

import com.ap.listing.constants.ApiConstants;
import com.ap.listing.payload.request.BuyerImprovementRequest;
import com.ap.listing.service.BuyerService;
import com.bloggios.provider.payload.ModuleResponse;
import com.bloggios.provider.utils.ControllerHelper;
import lombok.RequiredArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/v1/buyer")
@RequiredArgsConstructor
public class BuyerController {

    private static final Logger LOGGER = LoggerFactory.getLogger(BuyerController.class);

    private final BuyerService buyerService;

    @PostMapping
    public ResponseEntity<ModuleResponse> manageImprovement(@RequestBody BuyerImprovementRequest buyerImprovementRequest) {
        return ControllerHelper.loggedResponse(
                ()-> buyerService.manageImprovement(buyerImprovementRequest),
                ApiConstants.MANAGE_IMPROVEMENT,
                LOGGER
        );
    }
}
