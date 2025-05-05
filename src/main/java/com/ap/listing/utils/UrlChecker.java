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

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: UrlChecker
 */

import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.concurrent.TimeUnit;

@UtilityClass
@Slf4j
public class UrlChecker {

    private static final int TIMEOUT = 4000;
    private static final int MAX_RETRIES = 3;

    public static boolean isUrlAvailable(String url) {
        int attempt = 0;

        while (attempt < MAX_RETRIES) {
            try {
                if (checkUrl(url, "HEAD") || checkUrl(url, "GET")) {
                    log.info("URL is available: {}", url);
                    return true;
                }
            } catch (IOException e) {
                log.error("Attempt {} failed for URL {}: {}", attempt + 1, url, e.getMessage(), e);
            }

            attempt++;

            try {
                TimeUnit.MILLISECONDS.sleep((long) Math.pow(2, attempt) * 100);
            } catch (InterruptedException e) {
                log.error("Backoff sleep interrupted: {}", e.getMessage(), e);
                Thread.currentThread().interrupt();
            }
        }

        log.warn("URL is not available after {} attempts: {}", MAX_RETRIES, url);
        return false;
    }

    private static boolean checkUrl(String urlString, String method) throws IOException {
        HttpURLConnection connection = (HttpURLConnection) new URL(urlString).openConnection();
        connection.setRequestMethod(method);
        connection.setConnectTimeout(TIMEOUT);
        connection.setReadTimeout(TIMEOUT);
        connection.setRequestProperty("User-Agent", "Mozilla/5.0 (Java) UrlChecker");
        int responseCode = connection.getResponseCode();
        log.info("Checked URL: {}, Method: {}, Response: {}", urlString, method, responseCode);
        return responseCode >= 200 && responseCode < 500;
    }
}
