package com.ap.listing.feign;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: DomainMetricsFeignClient
 */

import com.ap.listing.configuration.FeignConfig;
import com.ap.listing.configuration.RapidApiFeignConfig;
import com.ap.listing.payload.response.DomainMetricsFeignResponse;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

@FeignClient(
        name = "${feign-client.domain-metrics.name}",
        url = "${feign-client.domain-metrics.url}",
        configuration = {
                RapidApiFeignConfig.class,
                FeignConfig.class
        }
)
public interface DomainMetricsFeignClient {

    @GetMapping("/{domain}")
    DomainMetricsFeignResponse getDomainMetrics(@PathVariable String domain);
}
