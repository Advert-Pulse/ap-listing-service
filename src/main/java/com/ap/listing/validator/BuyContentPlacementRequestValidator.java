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
package com.ap.listing.validator;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: BuyContentPlacementRequestValidator
 */

import com.ap.listing.enums.ErrorData;
import com.ap.listing.enums.WebsitePublishingStatus;
import com.ap.listing.exception.BadRequestException;
import com.ap.listing.model.WebsitePublisher;
import com.ap.listing.payload.UrlAnchorTextPayload;
import com.ap.listing.payload.request.BuyContentPlacementRequest;
import com.ap.listing.utils.SecurityContextUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
@Slf4j
public class BuyContentPlacementRequestValidator {

    public void validate(BuyContentPlacementRequest buyContentPlacementRequest, WebsitePublisher websitePublisher) {
        if (websitePublisher.getUserId().equals(SecurityContextUtil.getLoggedInUser().getUserId()))
            throw new BadRequestException(ErrorData.CANNOT_PURCHASE_OWN_LISTED_ORDER);

        List<UrlAnchorTextPayload> urlAnchorTexts = buyContentPlacementRequest.getUrlAnchorTexts();

        if (urlAnchorTexts == null) {
            throw new BadRequestException(ErrorData.ONE_URL_ANCHOR_TEXT_NEEDED);
        }

        boolean hasExtraLinkPricing = websitePublisher.getExtraLinkPricing() != null;
        int maxAllowedLinks = hasExtraLinkPricing ? 3 : 1;

        if (urlAnchorTexts.size() > maxAllowedLinks) {
            throw new BadRequestException(
                    hasExtraLinkPricing
                            ? ErrorData.ONLY_THREE_URL_ANCHOR_TEXT_ALLOWED
                            : ErrorData.NO_EXTRA_LINKS_ALLOWED
            );
        }

        for (int i = 0; i < urlAnchorTexts.size(); i++) {
            UrlAnchorTextPayload payload = urlAnchorTexts.get(i);
            int position = i + 1;

            if (payload.getUrl() == null || payload.getUrl().isEmpty()) {
                throw new BadRequestException(
                        ErrorData.URL_MANDATORY_FOR_URL_ANCHOR_TEXT,
                        "urlAnchorTexts",
                        String.format("Please give URL at the %s place", position)
                );
            }

            if (payload.getAnchorText() == null || payload.getAnchorText().isEmpty()) {
                throw new BadRequestException(
                        ErrorData.ANCHOR_TEXT_MANDATORY_FOR_URL_ANCHOR_TEXT,
                        "urlAnchorTexts",
                        String.format("Please give Anchor Text at the %s place", position)
                );
            }
        }

//        validatePublisher(websitePublisher);
    }

    private void validatePublisher(WebsitePublisher websitePublisher) {
        if (!websitePublisher.getWebsitePublishingStatus().equalsIgnoreCase(WebsitePublishingStatus.APPROVED.name()))
            throw new BadRequestException(ErrorData.WEBSITE_PUBLISHER_NOT_APPROVED);
    }
}
