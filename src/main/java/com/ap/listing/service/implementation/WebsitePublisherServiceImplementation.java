package com.ap.listing.service.implementation;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: WebsitePublisherServiceImplementation
 */

import com.ap.listing.dao.repository.WebsitePublisherRepository;
import com.ap.listing.dao.repository.WebsiteRepository;
import com.ap.listing.enums.ErrorData;
import com.ap.listing.exception.BadRequestException;
import com.ap.listing.model.WebsitePublisher;
import com.ap.listing.payload.request.PublishWebsiteRequest;
import com.ap.listing.service.WebsitePublisherService;
import com.ap.listing.transformer.PublishWebsiteRequestToWebsitePublisherTransformer;
import com.ap.listing.utils.SecurityContextUtil;
import com.ap.listing.validator.PublishWebsiteRequestValidator;
import com.bloggios.provider.payload.ModuleResponse;
import com.bloggios.provider.utils.ValueCheckerUtil;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@RequiredArgsConstructor
@Slf4j
public class WebsitePublisherServiceImplementation implements WebsitePublisherService {

    private final PublishWebsiteRequestValidator publishWebsiteRequestValidator;
    private final WebsiteRepository websiteRepository;
    private final PublishWebsiteRequestToWebsitePublisherTransformer publishWebsiteRequestToWebsitePublisherTransformer;
    private final WebsitePublisherRepository websitePublisherRepository;

    @Override
    public ResponseEntity<ModuleResponse> publishSite(PublishWebsiteRequest publishWebsiteRequest, String websitePublisherId) {
        ValueCheckerUtil.isValidUUID(
                websitePublisherId,
                ()-> new BadRequestException(ErrorData.WEBSITE_PUBLISHER_ID_INVALID, "websitePublisherId")
        );
        WebsitePublisher websitePublisher = websitePublisherRepository.findById(websitePublisherId)
                .orElseThrow(() -> new BadRequestException(ErrorData.WEBSITE_PUBLISHER_NOT_FOUND, "websitePublisherId"));
        publishWebsiteRequestValidator.validate(publishWebsiteRequest);
        WebsitePublisher transformedWebsitePublisher = publishWebsiteRequestToWebsitePublisherTransformer.transform(publishWebsiteRequest, websitePublisher);
        WebsitePublisher websitePublisherResponse = websitePublisherRepository.save(transformedWebsitePublisher);
        log.info("Website publisher saved to database: {}", websitePublisherResponse);
        return ResponseEntity.ok(ModuleResponse
                .builder()
                .message("Website Published and currently in moderation process")
                .userId(UUID.fromString(SecurityContextUtil.getLoggedInUserOrThrow().getUserId()))
                .id(UUID.fromString(websitePublisherResponse.getWebsitePublisherId()))
                .build());
    }
}
