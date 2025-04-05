package com.ap.listing.exception;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: AuthenticationException
 */

import com.ap.listing.enums.ErrorData;
import org.springframework.http.HttpStatus;

public class AuthenticationException extends BaseException {
    public AuthenticationException(ErrorData errorData, HttpStatus httpStatus) {
        this.setErrorData(errorData);
        this.setMessage(errorData.getMessage());
        this.setErrorCode(errorData.getCode());
        this.setCause(super.getCause());
        this.setLocalizedMessage(super.getLocalizedMessage());
        this.setClassName(super.getClass().getSimpleName());
        this.setHttpStatus(httpStatus);
    }
}
