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
package com.ap.listing.transformer;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: PrepareTask
 */

import com.ap.listing.enums.BuyerTaskStatus;
import com.ap.listing.enums.ProductType;
import com.ap.listing.enums.PublisherTaskStatus;
import com.ap.listing.model.TaskBuyer;
import com.ap.listing.model.TaskPublisher;
import com.ap.listing.model.WebsitePublisher;
import com.ap.listing.payload.BuyerTaskStatusPayload;
import com.ap.listing.payload.PricingPayload;
import com.ap.listing.payload.PublisherTaskStatusPayload;
import com.ap.listing.payload.UrlAnchorTextPayload;
import com.ap.listing.payload.request.BuyContentPlacementRequest;
import com.ap.listing.properties.AdvertPulseProperties;
import com.ap.listing.utils.SecurityContextUtil;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.modelmapper.ModelMapper;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

@Component
@RequiredArgsConstructor
@Slf4j
public class PrepareTaskBuyContentPlacement {

    private final ModelMapper modelMapper;
    private final AdvertPulseProperties advertPulseProperties;

    public TaskPublisher preparePublisherTask(BuyContentPlacementRequest buyContentPlacementRequest, WebsitePublisher websitePublisher) {
        TaskPublisher taskPublisher = new TaskPublisher();
        Date now = new Date();
        List<PricingPayload> pricingPayloads = preparePricingPayload(buyContentPlacementRequest, websitePublisher);
        List<PricingPayload> finalPricingPayloads = prepareFinalPricingPayload(pricingPayloads);
        taskPublisher.setProductType(ProductType.CONTENT_PLACEMENT.name());
        taskPublisher.setPriceBreak(finalPricingPayloads);
        taskPublisher.setPlatformFee(preparePlatformFee(pricingPayloads));
        taskPublisher.setTotalPrice(prepareTotalPrice(finalPricingPayloads));
        taskPublisher.setCurrentStatus(PublisherTaskStatus.YOUR_ACCEPTANCE.name());
        taskPublisher.setTaskStatus(new ArrayList<>(List.of(PublisherTaskStatusPayload.builder().date(now).status(PublisherTaskStatus.YOUR_ACCEPTANCE).build())));
        taskPublisher.setIsSpecialTopic(Boolean.FALSE.toString());
        taskPublisher.setDateCreated(now);
        taskPublisher.setDateUpdated(now);
        taskPublisher.setContent(buyContentPlacementRequest.getContent());
        taskPublisher.setSpecialRequirements(buyContentPlacementRequest.getSpecialRequirements());
        taskPublisher.setUrlAnchorTexts(buyContentPlacementRequest.getUrlAnchorTexts());
        taskPublisher.setContentType("N/A");
        taskPublisher.setBuyerId(SecurityContextUtil.getLoggedInUserOrThrow().getUserId());
        taskPublisher.setPublisherId(websitePublisher.getUserId());
        taskPublisher.setSiteUrl(websitePublisher.getDomain());
        log.info("Task Publisher prepared with response {}", taskPublisher);
        return taskPublisher;
    }

    public TaskBuyer prepareBuyerTask(TaskPublisher taskPublisher) {
        TaskBuyer taskBuyer = modelMapper.map(taskPublisher, TaskBuyer.class);
        taskBuyer.setTaskStatus(new ArrayList<>(List.of(BuyerTaskStatusPayload.builder().date(new Date()).status(BuyerTaskStatus.PUBLISHER_APPROVAL).build())));
        taskBuyer.setCurrentStatus(BuyerTaskStatus.PUBLISHER_APPROVAL.name());
        log.info("Task Buyer prepared with response {}", taskBuyer);
        return taskBuyer;
    }

    private double prepareTotalPrice(List<PricingPayload> pricingPayloads) {
        return pricingPayloads
                .stream()
                .mapToDouble(PricingPayload::getPrice)
                .sum();
    }

    private List<PricingPayload> preparePricingPayload(BuyContentPlacementRequest buyContentPlacementRequest, WebsitePublisher websitePublisher) {
        List<UrlAnchorTextPayload> urlAnchorTexts = buyContentPlacementRequest.getUrlAnchorTexts();
        List<PricingPayload> pricingPayloads = new ArrayList<>();
        PricingPayload pricingPayload = PricingPayload
                .builder()
                .title("Content Placement")
                .category("Main Service")
                .price(websitePublisher.getContentPlacementPrice())
                .build();
        pricingPayloads.add(pricingPayload);

        if (urlAnchorTexts.size() > 1) {
            for (int i=0 ; i<urlAnchorTexts.size() ; i++) {
                PricingPayload payload = PricingPayload
                        .builder()
                        .title("URL " + (i + 1))
                        .category("Extra services")
                        .price(websitePublisher.getExtraLinkPricing())
                        .build();
                pricingPayloads.add(payload);
            }
        }

        return pricingPayloads;
    }

    private List<PricingPayload> prepareFinalPricingPayload(List<PricingPayload> pricingPayloads) {
        double totalPrice = prepareTotalPrice(pricingPayloads);
        double contentPlacement = advertPulseProperties.getPlatformFee().getContentPlacement();
        double platformFee = totalPrice/contentPlacement;
        PricingPayload payload = PricingPayload
                .builder()
                .title("Platform Fee")
                .category("Fee")
                .price(platformFee)
                .build();
        pricingPayloads.add(payload);
        log.info("Pricing Payload prepared with response {}", pricingPayloads);
        return pricingPayloads;
    }

    private double preparePlatformFee(List<PricingPayload> pricingPayloads) {
        double totalPrice = prepareTotalPrice(pricingPayloads);
        double contentPlacement = advertPulseProperties.getPlatformFee().getContentPlacement();
        return totalPrice/contentPlacement;
    }
}
