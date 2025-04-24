package com.ap.listing.feign;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: AhrefWebsiteTrafficFeignClient
 */

import com.ap.listing.configuration.FeignConfig;
import com.ap.listing.configuration.DomainMetricsFeignConfig;
import org.springframework.cloud.openfeign.FeignClient;

@FeignClient(
        name = "${feign-client.domain-metrics.name}",
        url = "${feign-client.domain-metrics.url}",
        configuration = {
                DomainMetricsFeignConfig.class,
                FeignConfig.class
        }
)
public class AhrefWebsiteTrafficFeignClient {
}
