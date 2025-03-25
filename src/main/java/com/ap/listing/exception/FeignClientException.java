package com.ap.listing.exception;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: FeignClientException
 */

import com.ap.listing.enums.ErrorData;
import lombok.Getter;
import lombok.Setter;
import org.springframework.http.HttpStatus;

@Getter
@Setter
public class FeignClientException extends BaseException {

    public FeignClientException(ErrorData errorData, String localizedMessage, HttpStatus httpStatus) {
        this.setErrorData(errorData);
        this.setMessage(errorData.getMessage());
        this.setErrorCode(errorData.getCode());
        this.setCause(super.getCause());
        this.setLocalizedMessage(localizedMessage);
        this.setClassName(super.getClass().getSimpleName());
        this.setHttpStatus(httpStatus);
    }
}
