package com.ap.listing.utils;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: SchedulerMessageUtil
 */

import lombok.experimental.UtilityClass;

import java.util.Objects;

@UtilityClass
public class MessageUtil {

    public static String getMessage(String oldMessage, String message) {
        if (Objects.isNull(oldMessage)) {
            return message;
        } else {
            return oldMessage + " , " + message;
        }
    }
}
