/**
 * Proprietary License Agreement
 * <p>
 * Copyright (c) 2025 Advert Pulse
 * <p>
 * This software is the confidential and proprietary property of Advert Pulse
 * and is licensed, not sold. The application owner is Advert Pulse, and the
 * developer and maintainer is Bloggios. Only authorized Bloggios administrators
 * are permitted to copy, modify, distribute, or sublicense this software under
 * the terms set forth in this agreement.
 * <p>
 * You may not:
 * 1. Copy, modify, distribute, or sublicense this software without express
 *    written permission from Advert Pulse or Bloggios.
 * 2. Reverse engineer, decompile, disassemble, or otherwise attempt to derive
 *    the source code of the software.
 * 3. Modify this license in any way, including but not limited to altering its
 *    terms, even by Advert Pulse or any other entity, without express written
 *    permission from Bloggios administrators. Bloggios is the creator of this
 *    license and retains exclusive rights to update or modify it.
 * 4. Update or modify the license without written permission from Bloggios
 *    administrators.
 * <p>
 * The software is provided "as is," and Advert Pulse makes no warranties,
 * express or implied, regarding the software, including but not limited to any
 * warranties of merchantability, fitness for a particular purpose, or
 * non-infringement.
 * <p>
 * For inquiries regarding licensing, please contact support@bloggios.com.
 */
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
