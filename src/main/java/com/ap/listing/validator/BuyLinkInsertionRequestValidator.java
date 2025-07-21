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


import com.ap.listing.enums.ErrorData;
import com.ap.listing.exception.BadRequestException;
import com.ap.listing.payload.request.BuyLinkInsertionRequest;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class BuyLinkInsertionRequestValidator {
    public void validate(BuyLinkInsertionRequest buyLinkInsertionRequest) {

        if (buyLinkInsertionRequest.getSpecialRequirements() == null || buyLinkInsertionRequest.getSpecialRequirements().isEmpty()) {
            throw new BadRequestException(
                    ErrorData.SPECIAL_REQUIREMENT_MANDATORY,
                    "specialRequirementText",
                    "Please give special requirement."
            );
        }

        if (buyLinkInsertionRequest.getUrlAnchorTexts().getUrl() == null || buyLinkInsertionRequest.getUrlAnchorTexts().getUrl().isEmpty()) {
            throw new BadRequestException(
                    ErrorData.URL_MANDATORY_FOR_URL_ANCHOR_TEXT,
                    "urlText",
                    "Please give promoted url."
            );
        }

        if (buyLinkInsertionRequest.getUrlAnchorTexts().getAnchorText() == null || buyLinkInsertionRequest.getUrlAnchorTexts().getAnchorText().isEmpty()) {
            throw new BadRequestException(
                    ErrorData.ANCHOR_TEXT_MANDATORY_FOR_URL_ANCHOR_TEXT,
                    "urlAnchorText",
                    "Please give Anchor Text for promoted url."
            );
        }
    }

}
