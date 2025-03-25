package com.ap.listing.dao.repository;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: DomainMetricsRepository
 */

import com.ap.listing.model.DomainMetrics;
import org.springframework.data.jpa.repository.JpaRepository;

public interface DomainMetricsRepository extends JpaRepository<DomainMetrics, String> {
}
