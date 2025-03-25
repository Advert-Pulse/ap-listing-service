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

import com.bloggios.authprovider.authentication.AuthIdentity;
import com.bloggios.authprovider.enums.ErrorData;
import com.bloggios.authprovider.exception.BloggiosAuthenticationExceptionBloggios;
import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;

import java.util.Objects;

/**
 * Developer: Rohit Parihar
 * Project: bloggios-matching
 * GitHub: github.com/rohit-zip
 * File: SecurityContextUtil.java
 */

@UtilityClass
@Slf4j
public class SecurityContextUtil {

    public static AuthIdentity getLoggedInUser() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (Objects.nonNull(authentication) && authentication.getPrincipal() instanceof AuthIdentity authIdentity) {
            log.info("Request coming from userId: {}", authIdentity.getUserId());
            return authIdentity;
        }
        log.info("Request coming from unauthenticated user");
        return null;
    }

    public static AuthIdentity getLoggedInUserOrThrow() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (Objects.nonNull(authentication) && authentication.getPrincipal() instanceof AuthIdentity authIdentity) {
            log.info("Request coming from userId: {}", authIdentity.getUserId());
            return authIdentity;
        }
        log.info("Request coming from unauthenticated user -> Throwing Exception");
        throw new BloggiosAuthenticationExceptionBloggios(ErrorData.USER_NOT_LOGGED_IN);
    }
}
