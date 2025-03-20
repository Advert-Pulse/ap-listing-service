package com.ap.listing.service.implementation;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: PreferenceServiceImplementation
 */

import com.ap.listing.enums.ErrorData;
import com.ap.listing.exception.BadRequestException;
import com.ap.listing.payload.response.PreferenceResponse;
import com.ap.listing.service.PreferenceService;
import com.bloggios.provider.payload.ModuleResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
@Slf4j
public class PreferenceServiceImplementation implements PreferenceService {

    @Override
    public ResponseEntity<ModuleResponse> addPreference(String preference) {
        log.info("{} >> addPreference", getClass().getSimpleName());
        if (preference == null || preference.isEmpty()) {
            throw new BadRequestException(ErrorData.INVALID_PREFERENCE, "preference");
        }
        return null;
    }

    @Override
    public ResponseEntity<PreferenceResponse> getPreference() {
        return null;
    }
}
