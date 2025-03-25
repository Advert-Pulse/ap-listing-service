package com.ap.listing.transformer;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: DomainMetricsFeignResponseToDomainMetricsTransformer
 */

import com.ap.listing.model.DomainMetrics;
import com.ap.listing.model.Website;
import com.ap.listing.payload.response.DomainMetricsFeignResponse;
import lombok.RequiredArgsConstructor;
import org.modelmapper.ModelMapper;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
public class DomainMetricsFeignResponseToDomainMetricsTransformer {

    private final ModelMapper modelMapper;

    public DomainMetrics transform(DomainMetricsFeignResponse domainMetricsFeignResponse, Website website) {
        DomainMetrics domainMetrics = modelMapper.map(domainMetricsFeignResponse, DomainMetrics.class);
        domainMetrics.setWebsite(website);
        return domainMetrics;
    }
}
