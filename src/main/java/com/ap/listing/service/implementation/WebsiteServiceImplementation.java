package com.ap.listing.service.implementation;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: WebsiteServiceImplementation
 */

import com.ap.listing.dao.repository.WebsiteRepository;
import com.ap.listing.enums.ErrorData;
import com.ap.listing.exception.BadRequestException;
import com.ap.listing.feign.DomainMetricsFeignClient;
import com.ap.listing.model.Website;
import com.ap.listing.payload.response.DomainMetricsFeignResponse;
import com.ap.listing.service.WebsiteService;
import com.ap.listing.transformer.WebsiteTransformer;
import com.ap.listing.utils.ExtractBaseUrl;
import com.ap.listing.utils.UrlChecker;
import com.bloggios.provider.payload.ModuleResponse;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
public class WebsiteServiceImplementation implements WebsiteService {

    private final DomainMetricsFeignClient domainMetricsFeignClient;
    private final WebsiteTransformer websiteTransformer;
    private final WebsiteRepository websiteRepository;

    @Override
    @Transactional
    public ResponseEntity<ModuleResponse> addWebsite(String website) {
        String baseUrl = ExtractBaseUrl.extractBaseUrl(website);
        boolean urlAvailable = UrlChecker.isUrlAvailable(baseUrl);
        if (!urlAvailable) {
            throw new BadRequestException(ErrorData.WEBSITE_IRRESPONSIVE, "website");
        }
        DomainMetricsFeignResponse domainMetrics = domainMetricsFeignClient.getDomainMetrics(baseUrl);
        Website websiteEntity = websiteTransformer.transform(baseUrl);
        Website websiteResponse = websiteRepository.save(websiteEntity);

        return null;
    }

    @Override
    public ResponseEntity<ModuleResponse> addMultipleWebsite(List<String> websites) {
        return null;
    }
}