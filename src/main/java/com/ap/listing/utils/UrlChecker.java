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
        return responseCode >= 200 && responseCode < 400;
    }
}
