package com.ap.listing.dao.repository;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: WebsiteCategoryRepository
 */

import com.ap.listing.model.WebsiteCategory;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface WebsiteCategoryRepository extends JpaRepository<WebsiteCategory, String> {
    Optional<WebsiteCategory> findByCategoryIgnoreCase(String category);
}
