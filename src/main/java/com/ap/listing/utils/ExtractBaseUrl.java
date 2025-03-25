package com.ap.listing.utils;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: ExtractBaseUrl
 */

import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;

import java.net.MalformedURLException;
import java.net.URL;

@UtilityClass
@Slf4j
public class ExtractBaseUrl {

    public static String extractBaseUrl(String urlString) {
        log.info("{} >> extractBaseUrl -> urlString: {}", ExtractBaseUrl.class.getSimpleName(), urlString);
        try {
            URL url = new URL(urlString);
            return url.getProtocol() + "://" + url.getHost();
        } catch (MalformedURLException e) {
            if (!urlString.startsWith("http")) {
                int slashIndex = urlString.indexOf("/");
                return (slashIndex != -1) ? urlString.substring(0, slashIndex) : urlString;
            }
            return "Invalid URL";
        }
    }
}
