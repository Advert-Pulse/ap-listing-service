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
package com.ap.listing.controller;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: TestApiController
 */

import com.ap.listing.dao.repository.SchedulerRepository;
import com.ap.listing.enums.ScheduleTaskType;
import com.ap.listing.feign.AhrefFeignClient;
import com.ap.listing.model.Scheduler;
import com.ap.listing.payload.AhrefWebsiteAuthorityCheckerResponse;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.SneakyThrows;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Date;
import java.util.List;

@RestController
@RequestMapping("/test/data")
public class TestApiController {

    private final SchedulerRepository schedulerRepository;
    private final AhrefFeignClient ahrefFeignClient;

    public TestApiController(SchedulerRepository schedulerRepository, AhrefFeignClient ahrefFeignClient) {
        this.schedulerRepository = schedulerRepository;
        this.ahrefFeignClient = ahrefFeignClient;
    }

    @GetMapping
    @SneakyThrows
    public Object listItems() {
//        ResponseEntity<String> backlinkResponse = ahrefFeignClient.getBacklink("thehindu.com");
//        if (backlinkResponse.getStatusCode().is2xxSuccessful()) {
//            String body = backlinkResponse.getBody();
//            ObjectMapper mapper = new ObjectMapper();
//            AhrefWebsiteAuthorityCheckerResponse ahrefWebsiteAuthorityCheckerResponse = mapper.readValue(body, AhrefWebsiteAuthorityCheckerResponse.class);
//            return ahrefWebsiteAuthorityCheckerResponse;
//        }
//        return null;
        return ahrefFeignClient.getBacklinkResponse("thehindu.com");
    }

    @PostMapping
    public Scheduler addData(@RequestBody Scheduler scheduler) {
        scheduler.setCreatedOn(new Date());
        scheduler.setScheduledOn(new Date());
        scheduler.setUpdatedOn(new Date());
        return schedulerRepository.save(scheduler);
    }
}
