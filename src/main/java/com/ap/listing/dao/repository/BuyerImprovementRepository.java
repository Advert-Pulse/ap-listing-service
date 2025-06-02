package com.ap.listing.dao.repository;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: BuyerImprovementRepository
 */

import com.ap.listing.enums.BuyerImprovementStatus;
import com.ap.listing.model.BuyerImprovement;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface BuyerImprovementRepository extends JpaRepository<BuyerImprovement, String> {
    Optional<BuyerImprovement> findByTaskIdAndBuyerImprovementStatus(String taskId, BuyerImprovementStatus buyerImprovementStatus);
}
