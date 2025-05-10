package com.ap.listing.dao.repository;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: OrderArticleRepository
 */

import com.ap.listing.model.OrderArticle;
import org.springframework.data.jpa.repository.JpaRepository;

public interface OrderArticleRepository extends JpaRepository<OrderArticle, String> {
}
