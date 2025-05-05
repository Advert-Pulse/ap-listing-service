package com.ap.listing.feign;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: SimilarWebFeignClient
 */

import com.ap.listing.configuration.FeignConfig;
import com.ap.listing.configuration.FeignInterceptors;
import com.ap.listing.payload.SimilarWebTrafficHistoryWrapper;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(
        name = "${feign-client.similar-web.analytics.name}",
        url = "${feign-client.similar-web.analytics.url}",
        configuration = {
                FeignInterceptors.class,
                FeignConfig.class
        }
)
public interface SimilarWebFeignClient {

    @GetMapping("/analyticsv1")
    SimilarWebTrafficHistoryWrapper getWebsiteTraffic(@RequestParam String url);
}
