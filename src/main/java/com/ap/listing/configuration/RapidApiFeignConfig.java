package com.ap.listing.configuration;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: RapidApiFeignConfig
 */

import feign.RequestInterceptor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class RapidApiFeignConfig {

    @Value("${feign-client.rapid-api.host}")
    private String host;

    @Value("${feign-client.rapid-api.api-key}")
    private String apiKey;

    @Bean
    public RequestInterceptor requestInterceptor() {
        return requestTemplate -> {
            requestTemplate.header("x-rapidapi-host", host);
            requestTemplate.header("x-rapidapi-key", apiKey);
        };
    }
}
