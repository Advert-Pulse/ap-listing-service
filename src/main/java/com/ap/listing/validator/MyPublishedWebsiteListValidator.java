package com.ap.listing.validator;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: MyPublishedWebsiteListValidator
 */

import com.ap.listing.enums.ErrorData;
import com.ap.listing.exception.BadRequestException;
import com.bloggios.query.payload.ListPayload;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.Objects;

@Component
@Slf4j
public class MyPublishedWebsiteListValidator {

    public void validate(ListPayload listPayload) {
        if (Objects.nonNull(listPayload) && Objects.nonNull(listPayload.getFilters())) {
            listPayload.getFilters().forEach(filter -> {
                if (filter.getFilterKey().equals("userId")) {
                    throw new BadRequestException(ErrorData.MY_PUBLISHED_SITE_USER_ID_FILTER);
                }
            });
        }
    }
}
