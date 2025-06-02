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
import com.bloggios.provider.payload.ExceptionResponse;
import com.bloggios.provider.payload.ModuleResponse;
import com.bloggios.provider.utils.ControllerHelper;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import lombok.RequiredArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/v1/buyer")
@RequiredArgsConstructor
public class BuyerController {

    private static final Logger LOGGER = LoggerFactory.getLogger(BuyerController.class);

    private final BuyerService buyerService;

    @Operation(
            responses = {
                    @ApiResponse(description = "SUCCESS", responseCode = "200", content = @Content(
                            mediaType = "application/json", schema = @Schema(implementation = ModuleResponse.class)
                    )),
                    @ApiResponse(description = "No Content", responseCode = "401", content = {
                            @Content(schema = @Schema())
                    }),
                    @ApiResponse(description = "FORBIDDEN", responseCode = "403", content = {
                            @Content(mediaType = "application/json", schema = @Schema(implementation = String.class))
                    }),
                    @ApiResponse(description = "BAD REQUEST", responseCode = "400", content = {
                            @Content(mediaType = "application/json", schema = @Schema(implementation = ExceptionResponse.class))
                    })
            }
    )
    @PostMapping("/manage-improvement")
    public ResponseEntity<ModuleResponse> manageImprovement(@RequestBody BuyerImprovementRequest buyerImprovementRequest) {
        return ControllerHelper.loggedResponse(
                ()-> buyerService.manageImprovement(buyerImprovementRequest),
                ApiConstants.MANAGE_IMPROVEMENT,
                LOGGER
        );
    }

    @Operation(
            responses = {
                    @ApiResponse(description = "SUCCESS", responseCode = "200", content = @Content(
                            mediaType = "application/json", schema = @Schema(implementation = ModuleResponse.class)
                    )),
                    @ApiResponse(description = "No Content", responseCode = "401", content = {
                            @Content(schema = @Schema())
                    }),
                    @ApiResponse(description = "FORBIDDEN", responseCode = "403", content = {
                            @Content(mediaType = "application/json", schema = @Schema(implementation = String.class))
                    }),
                    @ApiResponse(description = "BAD REQUEST", responseCode = "400", content = {
                            @Content(mediaType = "application/json", schema = @Schema(implementation = ExceptionResponse.class))
                    })
            }
    )
    @GetMapping("/manage-completed/{taskId}")
    public ResponseEntity<ModuleResponse> manageCompleted(@PathVariable String taskId) {
        return ControllerHelper.loggedResponse(
                ()-> buyerService.manageCompleted(taskId),
                ApiConstants.MANAGE_COMPLETED,
                LOGGER
        );
    }
}
