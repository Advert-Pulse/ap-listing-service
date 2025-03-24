package com.ap.listing.controller;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: WebsiteApi
 */

import com.bloggios.provider.payload.ExceptionResponse;
import com.bloggios.provider.payload.ModuleResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.List;

@RequestMapping("/website")
public interface WebsiteApi {

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
    @PostMapping(
            value = "/add-single",
            consumes = MediaType.APPLICATION_FORM_URLENCODED_VALUE,
            produces = MediaType.APPLICATION_JSON_VALUE
    )
    ResponseEntity<ModuleResponse> addWebsite(@RequestParam String website);

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
    @PostMapping("/add-multiple")
    ResponseEntity<ModuleResponse> addMultipleWebsite(@RequestParam List<String> website);
}
