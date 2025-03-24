package com.ap.listing.controller.implementation;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: WebsiteApiController
 */

import com.ap.listing.controller.WebsiteApi;
import com.bloggios.provider.payload.ModuleResponse;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@RequiredArgsConstructor
public class WebsiteApiController implements WebsiteApi {

    @Override
    public ResponseEntity<ModuleResponse> addWebsite(String website) {
        return null;
    }

    @Override
    public ResponseEntity<ModuleResponse> addMultipleWebsite(List<String> website) {
        return null;
    }
}
