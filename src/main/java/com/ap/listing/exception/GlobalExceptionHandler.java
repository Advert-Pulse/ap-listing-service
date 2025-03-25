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

import com.ap.listing.enums.ErrorData;
import com.ap.listing.utils.ErrorResponseGeneratorUtil;
import com.bloggios.provider.payload.ExceptionResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

/**
 * Developer: Rohit Parihar
 * Project: bloggios-matching
 * GitHub: github.com/rohit-zip
 * File: GlobalExceptionHandler.java
 */

@RestControllerAdvice
@Slf4j
@Order(Ordered.HIGHEST_PRECEDENCE)
public class GlobalExceptionHandler {

    @ExceptionHandler(BadRequestException.class)
    public ResponseEntity<ExceptionResponse> badRequestException(BadRequestException exception) {
        log.error("BadRequestException Occurred >> {}", exception.toString());
        ExceptionResponse exceptionResponse = ErrorResponseGeneratorUtil.generate(exception);
        return new ResponseEntity<>(
                exceptionResponse,
                exception.getHttpStatus()
        );
    }

    @ExceptionHandler(FeignClientException.class)
    public ResponseEntity<ExceptionResponse> feignClientException(FeignClientException exception) {
        log.error("FeignClientException Occurred >> {}", exception.toString());
        ExceptionResponse exceptionResponse = ErrorResponseGeneratorUtil.generate(exception);
        return new ResponseEntity<>(
                exceptionResponse,
                exception.getHttpStatus()
        );
    }

    @ExceptionHandler(InternalException.class)
    public ResponseEntity<ExceptionResponse> internalException(InternalException exception) {
        log.error("InternalException Occurred >> {}", exception.toString());
        ExceptionResponse exceptionResponse = ErrorResponseGeneratorUtil.generate(exception);
        return new ResponseEntity<>(
                exceptionResponse,
                exception.getHttpStatus()
        );
    }

    @ExceptionHandler(Exception.class)
    public ResponseEntity<ExceptionResponse> exception(Exception exception) {
        log.error("Exception Occurred >> {}", exception.toString());
        ExceptionResponse exceptionResponse = ExceptionResponse
                .builder()
                .message(ErrorData.INTERNAL_ERROR.getMessage())
                .code(ErrorData.INTERNAL_ERROR.getCode())
                .httpStatus(HttpStatus.INTERNAL_SERVER_ERROR.getReasonPhrase())
                .localizedMessage(exception.getMessage())
                .build();
        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(exceptionResponse);
    }
}
