package com.ap.listing.dao.repository;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: WebsitePublisherRepository
 */

import com.ap.listing.model.Website;
import com.ap.listing.model.WebsitePublisher;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface WebsitePublisherRepository extends JpaRepository<WebsitePublisher, String> {

    Optional<WebsitePublisher> findByWebsiteAndUserId(Website website, String userId);
}
