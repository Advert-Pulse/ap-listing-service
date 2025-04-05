package com.ap.listing.utils;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: StringCapitalizer
 */

import lombok.experimental.UtilityClass;

@UtilityClass
public class StringCapitalizer {

    public static String formatText(String input) {
        if (input == null || input.isEmpty()) return input;
        String cleaned = input.trim().replaceAll("\\s+", " ");

        StringBuilder result = new StringBuilder();
        for (String word : cleaned.split(" ")) {
            if (!word.isEmpty()) {
                result.append(Character.toUpperCase(word.charAt(0)));
                result.append(word.substring(1).toLowerCase());
                result.append(" ");
            }
        }
        return result.toString().trim();
    }
}
