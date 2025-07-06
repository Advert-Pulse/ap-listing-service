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
package com.ap.listing.service.implementation;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: PreferenceServiceImplementation
 */

import com.ap.listing.constants.ServiceConstants;
import com.ap.listing.dao.repository.PreferenceRepository;
import com.ap.listing.enums.ErrorData;
import com.ap.listing.exception.BadRequestException;
import com.ap.listing.model.Preference;
import com.ap.listing.payload.response.PreferenceResponse;
import com.ap.listing.service.PreferenceService;
import com.ap.listing.transformer.PreferenceTransformer;
import com.ap.listing.utils.SecurityContextUtil;
import com.bloggios.provider.payload.ModuleResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.Set;
import java.util.UUID;

@Service
@RequiredArgsConstructor
@Slf4j
public class PreferenceServiceImplementation implements PreferenceService {

    private final PreferenceTransformer preferenceTransformer;
    private final PreferenceRepository preferenceRepository;

    @Override
    public ResponseEntity<ModuleResponse> addPreference(String preference) {
        log.info("{} >> addPreference", getClass().getSimpleName());
        Set<String> validPreferences = Set.of(ServiceConstants.BUYER, ServiceConstants.SELLER);
        if (!validPreferences.contains(preference)) {
            throw new BadRequestException(ErrorData.INVALID_PREFERENCE, "preference");
        }

        Preference preferenceEntity = preferenceTransformer.transform(preference);
        Preference preferenceResponse = preferenceRepository.saveAndFlush(preferenceEntity);
        log.info("{} >> addPreference -> Preference Saved : {}", getClass().getSimpleName(), preferenceResponse);
        ModuleResponse response = new ModuleResponse(
                "Preference has been set to " + preferenceResponse.getPreferenceType(),
                UUID.fromString(preferenceResponse.getUserId()),
                UUID.fromString(preferenceResponse.getPreferenceId())
        );
        return ResponseEntity.ok(response);
    }

    @Override
    public ResponseEntity<PreferenceResponse> getPreference() {
        log.info("{} >> getPreference", getClass().getSimpleName());
        String userId = SecurityContextUtil.getLoggedInUserOrThrow().getUserId();
        Preference preference = preferenceRepository.findByUserId(userId)
                .orElseThrow(() -> new BadRequestException(ErrorData.PREFERENCE_NOT_ADDED));
        log.info("{} >> getPreference -> Fetched Preference: {}", getClass().getSimpleName(), preference);
        return ResponseEntity.ok(
                PreferenceResponse
                        .builder()
                        .id(preference.getPreferenceId())
                        .preference(preference.getPreferenceType())
                        .userId(preference.getUserId())
                        .build()
        );
    }
}
