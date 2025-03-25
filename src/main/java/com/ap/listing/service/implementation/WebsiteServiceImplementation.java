package com.ap.listing.service.implementation;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: WebsiteServiceImplementation
 */

import com.ap.listing.service.WebsiteService;
import com.bloggios.provider.payload.ModuleResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
public class WebsiteServiceImplementation implements WebsiteService {

    @Override
    public ResponseEntity<ModuleResponse> addWebsite(String website) {

        return null;
    }

    @Override
    public ResponseEntity<ModuleResponse> addMultipleWebsite(List<String> websites) {
        return null;
    }
}