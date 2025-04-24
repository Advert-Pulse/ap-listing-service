package com.ap.listing.feign;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: AhrefFeignClient
 */

import com.ap.listing.configuration.FeignConfig;
import com.ap.listing.configuration.FeignInterceptors;
import com.ap.listing.payload.response.AhrefWebsiteTrafficResponse;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(
        name = "${feign-client.ahref.website-traffic.name}",
        url = "${feign-client.ahref.website-traffic.url}",
        configuration = {
                FeignInterceptors.class,
                FeignConfig.class
        }
)
public interface AhrefFeignClient {

    @GetMapping("/traffic")
    AhrefWebsiteTrafficResponse getWebsiteTraffic(@RequestParam String url);
}
