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
package com.ap.listing.dao.query;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: WebsiteFilter
 */

import com.ap.listing.model.WebsiteCategory;
import com.ap.listing.model.WebsiteData;
import com.bloggios.query.payload.Filter;
import com.bloggios.query.payload.ListPayload;
import com.bloggios.query.query.InitQuery;
import jakarta.persistence.criteria.Join;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import org.springframework.stereotype.Component;
import org.springframework.util.ObjectUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

@Component
public class WebsiteFilter extends InitQuery<WebsiteData> {

    @Override
    public List<Predicate> initFilter(ListPayload listPayload, Root<WebsiteData> root) {
        List<Predicate> filterPredicates = new ArrayList<>();
        if (!ObjectUtils.isEmpty(listPayload.getFilters())) {
            listPayload
                    .getFilters()
                    .forEach(filter -> {
                        List<Predicate> selectionPredicates = new ArrayList<>();
                        if (Objects.nonNull(filter.getNestedPath())) {
                            Join<WebsiteData, WebsiteCategory> roleJoin = root.join(filter.getNestedPath());
                            if (filter.isPartialSearch()) {
                                filter.getSelections().forEach(selection ->
                                        selectionPredicates.add(this.getCriteriaBuilder().like(
                                                getCriteriaBuilder().lower(roleJoin.get(filter.getFilterKey())),
                                                "%" + selection.toString().toLowerCase() + "%"
                                        ))
                                );
                                filterPredicates.add(getCriteriaBuilder().or(selectionPredicates.toArray(new Predicate[0])));
                            } else {
                                filterPredicates.add(roleJoin.get(filter.getFilterKey()).in(filter.getSelections()));
                            }
                        } else {
                            normalFilter(root, filter, selectionPredicates, filterPredicates);
                        }
                    });
        }
        return filterPredicates;
    }

    private void normalFilter(Root<WebsiteData> root, Filter filter, List<Predicate> selectionPredicates, List<Predicate> filterPredicates) {
        if (filter.isPartialSearch()) {
            filter
                    .getSelections()
                    .forEach(selection ->
                            selectionPredicates
                                    .add(getCriteriaBuilder()
                                            .like(getCriteriaBuilder()
                                                            .lower(root.get(filter.getFilterKey())),
                                                    "%" + selection.toString().toLowerCase() + "%"))
                    );
            filterPredicates
                    .add(
                            getCriteriaBuilder()
                                    .or(selectionPredicates.toArray(new Predicate[0]))
                    );
        } else {
            filterPredicates
                    .add(root
                            .get(filter.getFilterKey())
                            .in(filter.getSelections())
                    );
        }
    }
}

