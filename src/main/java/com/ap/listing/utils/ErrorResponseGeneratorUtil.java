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
package com.ap.listing.utils;

import com.ap.listing.exception.BaseException;
import com.bloggios.provider.constants.ServiceConstants;
import com.bloggios.provider.payload.ExceptionResponse;
import lombok.experimental.UtilityClass;
import org.slf4j.MDC;

/**
 * Developer: Rohit Parihar
 * Project: bloggios-matching
 * GitHub: github.com/rohit-zip
 * File: ErrorResponseGeneratorUtil.java
 */

@UtilityClass
public class ErrorResponseGeneratorUtil {

    public ExceptionResponse generate(BaseException exception) {
        String breadcrumbId = MDC.get(ServiceConstants.BREADCRUMB_ID);
        return ExceptionResponse
                .builder()
                .message(exception.getMessage())
                .field(exception.getField())
                .breadcrumbId(breadcrumbId)
                .code(exception.getErrorCode())
                .httpStatus(exception.getHttpStatus().getReasonPhrase())
                .localizedMessage(exception.getLocalizedMessage())
                .build();
    }
}
