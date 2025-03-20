package com.ap.listing.dao.repository;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: PreferenceRepository
 */

import com.ap.listing.model.Preference;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface PreferenceRepository extends JpaRepository<Preference, String> {

    Optional<Preference> findByUserId(String userId);
}
