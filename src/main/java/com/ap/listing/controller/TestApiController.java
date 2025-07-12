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
