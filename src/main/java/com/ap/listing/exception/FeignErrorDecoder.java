package com.ap.listing.exception;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: FeignErrorDecoder
 */

import com.ap.listing.enums.ErrorData;
import feign.Response;
import feign.codec.ErrorDecoder;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Component;

import java.io.IOException;

@Component
@Slf4j
public class FeignErrorDecoder implements ErrorDecoder {

    @Override
    public Exception decode(String methodKey, Response response) {
        HttpStatus status = HttpStatus.valueOf(response.status());
        String responseBody = getResponseBody(response);

        log.error("Feign client error: method={}, status={}, body={}",
                methodKey, status, responseBody);

        return switch (status) {
            case NOT_FOUND -> new FeignClientException(
                    ErrorData.FEIGN_RESOURCE_NOT_FOUND,
                    responseBody,
                    status
            );
            case BAD_REQUEST -> new FeignClientException(
                    ErrorData.FEIGN_BAD_REQUEST_EXCEPTION,
                    responseBody,
                    status
            );
            case UNAUTHORIZED -> new FeignClientException(
                    ErrorData.FEIGN_UNAUTHORIZED,
                    responseBody,
                    status
            );
            case FORBIDDEN -> new FeignClientException(
                    ErrorData.FEIGN_FORBIDDEN,
                    responseBody,
                    status
            );
            default -> new FeignClientException(
                    ErrorData.FEIGN_INTERNAL_SERVER_ERROR,
                    responseBody,
                    status
            );
        };
    }

    private String getResponseBody(Response response) {
        if (response.body() != null) {
            try {
                return new String(response.body().asInputStream().readAllBytes());
            } catch (IOException e) {
                log.error("Failed to read response body", e);
            }
        }
        return "No response body";
    }
}
