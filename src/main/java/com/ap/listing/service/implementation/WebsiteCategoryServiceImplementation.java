package com.ap.listing.service.implementation;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: WebsiteCategoryServiceImplementation
 */

import com.ap.listing.dao.repository.WebsiteCategoryRepository;
import com.ap.listing.enums.ErrorData;
import com.ap.listing.exception.BadRequestException;
import com.ap.listing.model.WebsiteCategory;
import com.ap.listing.payload.request.WebsiteCategoryRequest;
import com.ap.listing.payload.response.ListResponse;
import com.ap.listing.payload.response.WebsiteCategoryResponse;
import com.ap.listing.service.WebsiteCategoryService;
import com.ap.listing.transformer.WebsiteCategoryRequestTransformer;
import com.ap.listing.transformer.WebsiteCategoryResponseTransformer;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.Optional;

@Service
@RequiredArgsConstructor
@Slf4j
public class WebsiteCategoryServiceImplementation implements WebsiteCategoryService {

    private final WebsiteCategoryRepository websiteCategoryRepository;
    private final WebsiteCategoryRequestTransformer websiteCategoryRequestTransformer;
    private final WebsiteCategoryResponseTransformer websiteCategoryResponseTransformer;

    @Override
    public ResponseEntity<WebsiteCategoryResponse> addWebsiteCategory(WebsiteCategoryRequest websiteCategoryRequest) {
        Optional<WebsiteCategory> byCategoryIgnoreCase = websiteCategoryRepository.findByCategoryIgnoreCase(websiteCategoryRequest.getCategory());
        if (byCategoryIgnoreCase.isPresent()) {
            throw new BadRequestException(ErrorData.WEBSITE_CATEGORY_ALREADY_PRESENT);
        }
        WebsiteCategory websiteCategoryTransformed = websiteCategoryRequestTransformer.transform(websiteCategoryRequest);
        WebsiteCategory websiteCategoryResponse = websiteCategoryRepository.save(websiteCategoryTransformed);
        log.info("Website category saved with response: {}", websiteCategoryResponse);
        return ResponseEntity.ok(websiteCategoryResponseTransformer.transform(websiteCategoryResponse));
    }

    @Override
    public ResponseEntity<ListResponse> getWebsiteCategoryList() {
        return null;
    }
}
