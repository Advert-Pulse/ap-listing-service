package com.ap.listing.dao.repository;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: WebsiteRepository
 */

import com.ap.listing.model.Website;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface WebsiteRepository extends JpaRepository<Website, String> {
    Optional<Website> findByDomain(String domain);
}
