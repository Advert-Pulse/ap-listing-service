package com.ap.listing.controller;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: TestApiController
 */

import com.ap.listing.dao.repository.SchedulerRepository;
import com.ap.listing.enums.ScheduleTaskType;
import com.ap.listing.model.Scheduler;
import org.springframework.web.bind.annotation.*;

import java.util.Date;
import java.util.List;

@RestController
@RequestMapping("/test/data")
public class TestApiController {

    private final SchedulerRepository schedulerRepository;

    public TestApiController(SchedulerRepository schedulerRepository) {
        this.schedulerRepository = schedulerRepository;
    }

    @GetMapping
    public List<Scheduler> listItems() {
        return schedulerRepository.findAllByScheduledTaskTypeAndIsSchedulingDoneAndScheduledOnBefore(
                ScheduleTaskType.FETCH_WEBSITE_DATA,
                Boolean.FALSE,
                new Date()
        );
    }

    @PostMapping
    public Scheduler addData(@RequestBody Scheduler scheduler) {
        scheduler.setCreatedOn(new Date());
        scheduler.setScheduledOn(new Date());
        scheduler.setUpdatedOn(new Date());
        return schedulerRepository.save(scheduler);
    }
}
