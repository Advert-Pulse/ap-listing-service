package com.ap.listing.transformer;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: PreferenceTransformer
 */

import com.ap.listing.model.Preference;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

@Component
@Slf4j
public class PreferenceTransformer {

    public Preference transform(String preference) {
        log.info("{} >> transform", getClass().getSimpleName());
        return Preference
                .builder()
                .userId(SecurityContextUtil.getLoggedInUserOrThrow().getUserId())
                .preferenceType(preference.trim().toLowerCase())
                .build();
    }
}
