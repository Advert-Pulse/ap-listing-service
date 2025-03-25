package com.ap.listing.service.implementation;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: WebsiteServiceImplementation
 */

import com.ap.listing.constants.ServiceConstants;
import com.ap.listing.dao.repository.DomainMetricsRepository;
import com.ap.listing.dao.repository.WebsiteRepository;
import com.ap.listing.enums.ErrorData;
import com.ap.listing.exception.BadRequestException;
import com.ap.listing.feign.DomainMetricsFeignClient;
import com.ap.listing.model.DomainMetrics;
import com.ap.listing.model.Website;
import com.ap.listing.payload.response.DomainMetricsFeignResponse;
import com.ap.listing.processor.WebsiteDefaultPublisherProcessor;
import com.ap.listing.service.WebsiteService;
import com.ap.listing.transformer.DomainMetricsFeignResponseToDomainMetricsTransformer;
import com.ap.listing.transformer.WebsiteTransformer;
import com.ap.listing.utils.ExtractBaseUrl;
import com.ap.listing.utils.SecurityContextUtil;
import com.ap.listing.utils.UrlChecker;
import com.bloggios.provider.payload.ModuleResponse;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static com.ap.listing.constants.ServiceConstants.HTTPS;

@Service
@RequiredArgsConstructor
@Slf4j
public class WebsiteServiceImplementation implements WebsiteService {

    private final DomainMetricsFeignClient domainMetricsFeignClient;
    private final WebsiteTransformer websiteTransformer;
    private final WebsiteRepository websiteRepository;
    private final DomainMetricsFeignResponseToDomainMetricsTransformer domainMetricsFeignResponseToDomainMetricsTransformer;
    private final DomainMetricsRepository domainMetricsRepository;
    private final WebsiteDefaultPublisherProcessor websiteDefaultPublisherProcessor;

    @Override
    @Transactional
    public ResponseEntity<ModuleResponse> addWebsite(String website) {
        String domain = website.toLowerCase();
        if (!domain.startsWith(ServiceConstants.HTTP) && !domain.startsWith(HTTPS)) {
            domain = HTTPS + domain;
        }
        String baseUrl = ExtractBaseUrl.extractBaseUrl(domain);
        Optional<Website> byDomain = websiteRepository.findByDomain(domain);
        boolean urlAvailable = UrlChecker.isUrlAvailable(baseUrl);
        if (byDomain.isPresent()) {
            if (!urlAvailable) {
                Website websiteEntity = websiteTransformer.transformWebsiteNotAvailable(byDomain.get());
                websiteRepository.save(websiteEntity);
                throw new BadRequestException(ErrorData.WEBSITE_IRRESPONSIVE, "website");
            }
            websiteDefaultPublisherProcessor.process(byDomain.get());
            return ResponseEntity.ok(
                    ModuleResponse
                            .builder()
                            .message("Website is available to be added")
                            .userId(UUID.fromString(SecurityContextUtil.getLoggedInUserOrThrow().getUserId()))
                            .id(UUID.fromString(byDomain.get().getWebsiteId()))
                            .build()
            );
        } else {
            if (!urlAvailable) {
                throw new BadRequestException(ErrorData.WEBSITE_IRRESPONSIVE, "website");
            }
            String feignUrl;
            if (baseUrl.startsWith(HTTPS)) {
                feignUrl = baseUrl.substring(8);
            } else {
                feignUrl = baseUrl.substring(7);
            }
            DomainMetricsFeignResponse domainMetrics = domainMetricsFeignClient.getDomainMetrics(feignUrl);
            log.info("Domain Metrics Response: {}", domainMetrics);
            if (domainMetrics.getDomain() == null) {
                throw new BadRequestException(ErrorData.WEBSITE_IRRESPONSIVE, "website");
            }
            Website websiteEntity = websiteTransformer.transform(baseUrl);
            Website websiteResponse = websiteRepository.save(websiteEntity);
            log.info("Website Saved : {}", websiteResponse);
            DomainMetrics domainMetricsTransform = domainMetricsFeignResponseToDomainMetricsTransformer.transform(domainMetrics, websiteResponse);
            DomainMetrics domainMetricsResponse = domainMetricsRepository.save(domainMetricsTransform);
            log.info("Domain Metrics Saved : {}", domainMetricsResponse);
            websiteDefaultPublisherProcessor.process(websiteEntity);
            return ResponseEntity.ok(
                    ModuleResponse
                            .builder()
                            .message("Website has been added to server")
                            .userId(UUID.fromString(SecurityContextUtil.getLoggedInUserOrThrow().getUserId()))
                            .id(UUID.fromString(websiteResponse.getWebsiteId()))
                            .build()
            );
        }
    }

    @Override
    public ResponseEntity<ModuleResponse> addMultipleWebsite(List<String> websites) {
        return null;
    }
}