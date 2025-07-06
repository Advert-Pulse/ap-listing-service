package com.ap.listing.feign;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: GoogleFeignClient
 */

import com.ap.listing.payload.request.GoogleRefreshTokenResponse;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.util.MultiValueMap;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

@FeignClient(
        name = "${feign-client.google-service.name}",
        url = "${feign-client.google-service.url}"
)
public interface GoogleFeignClient {

    @PostMapping(
            value = "/token",
            consumes = MediaType.APPLICATION_FORM_URLENCODED_VALUE
    )
    GoogleRefreshTokenResponse exchangeAuthCodeForToken(@RequestBody MultiValueMap<String, String> formParams);
}
