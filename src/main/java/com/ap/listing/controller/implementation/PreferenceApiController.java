package com.ap.listing.controller.implementation;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: PreferenceApiImplementation
 */

import com.ap.listing.controller.PreferenceApi;
import com.ap.listing.payload.response.PreferenceResponse;
import com.ap.listing.service.PreferenceService;
import com.bloggios.provider.payload.ModuleResponse;
import com.bloggios.provider.utils.ControllerHelper;
import lombok.RequiredArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RestController;

import static com.ap.listing.constants.ApiConstants.ADD_PREFERENCE;
import static com.ap.listing.constants.ApiConstants.GET_PREFERENCE;

@RestController
@RequiredArgsConstructor
public class PreferenceApiController implements PreferenceApi {

    private static final Logger LOGGER = LoggerFactory.getLogger(PreferenceApiController.class);

    private final PreferenceService preferenceService;

    @Override
    public ResponseEntity<ModuleResponse> addPreference(String preference) {
        return ControllerHelper.loggedResponse(
                ()-> preferenceService.addPreference(preference),
                ADD_PREFERENCE,
                LOGGER
        );
    }

    @Override
    public ResponseEntity<PreferenceResponse> getPreference() {
        return ControllerHelper.loggedResponse(
                preferenceService::getPreference,
                GET_PREFERENCE,
                LOGGER
        );
    }
}
