package com.ap.listing.dao.repository;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: WebsiteRepository
 */

import com.ap.listing.model.Website;
import org.springframework.data.jpa.repository.JpaRepository;

public interface WebsiteRepository extends JpaRepository<Website, String> {
}
