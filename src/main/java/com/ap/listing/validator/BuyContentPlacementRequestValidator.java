package com.ap.listing.validator;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: BuyContentPlacementRequestValidator
 */

import com.ap.listing.enums.ErrorData;
import com.ap.listing.exception.BadRequestException;
import com.ap.listing.model.WebsitePublisher;
import com.ap.listing.payload.UrlAnchorTextPayload;
import com.ap.listing.payload.request.BuyContentPlacementRequest;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Objects;

@Component
@Slf4j
public class BuyContentPlacementRequestValidator {

    public void validate(BuyContentPlacementRequest buyContentPlacementRequest, WebsitePublisher websitePublisher) {
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
    }
}
