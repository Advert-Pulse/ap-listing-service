package com.ap.listing.processor;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: MyPublishedWebsiteUserIdFilterProcessor
 */

import com.ap.listing.utils.SecurityContextUtil;
import com.bloggios.query.payload.Filter;
import com.bloggios.query.payload.ListPayload;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

@Component
@Slf4j
public class MyPublishedWebsiteUserIdFilterProcessor {

    public ListPayload process(ListPayload listPayload) {
        List<Filter> filters = Optional.ofNullable(listPayload.getFilters())
                .filter(f -> !f.isEmpty())
                .orElse(new ArrayList<>());

        Filter userFilter = Filter.builder()
                .filterKey("userId")
                .selections(List.of(SecurityContextUtil.getLoggedInUserOrThrow().getUserId()))
                .build();

        filters.add(userFilter);
        listPayload.setFilters(filters);

        return listPayload;
    }
}
