package com.ap.listing.service.implementation;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: WebsitePublisherServiceImplementation
 */

import com.ap.listing.dao.repository.WebsiteRepository;
import com.ap.listing.enums.ErrorData;
import com.ap.listing.exception.BadRequestException;
import com.ap.listing.model.Website;
import com.ap.listing.payload.request.PublishWebsiteRequest;
import com.ap.listing.service.WebsitePublisherService;
import com.ap.listing.validator.PublishWebsiteRequestValidator;
import com.bloggios.provider.payload.ModuleResponse;
import com.bloggios.provider.utils.ValueCheckerUtil;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
@Slf4j
public class WebsitePublisherServiceImplementation implements WebsitePublisherService {

    private final PublishWebsiteRequestValidator publishWebsiteRequestValidator;
    private final WebsiteRepository websiteRepository;

    @Override
    public ResponseEntity<ModuleResponse> publishSite(PublishWebsiteRequest publishWebsiteRequest, String websiteId) {
        ValueCheckerUtil.isValidUUID(
                websiteId,
                ()-> new BadRequestException(ErrorData.WEBSITE_ID_INVALID, "websiteId")
        );
        publishWebsiteRequestValidator.validate(publishWebsiteRequest);
        Website website = websiteRepository.findById(websiteId)
                .orElseThrow(() -> new BadRequestException(ErrorData.WEBSITE_NOT_FOUND_BY_ID));

        return null;
    }
}
