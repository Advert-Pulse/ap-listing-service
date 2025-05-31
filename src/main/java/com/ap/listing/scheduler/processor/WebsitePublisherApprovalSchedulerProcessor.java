package com.ap.listing.scheduler.processor;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: WebsitePublisherApprovalSchedulerProcessor
 */

import com.ap.listing.dao.repository.SchedulerRepository;
import com.ap.listing.dao.repository.WebsitePublisherRepository;
import com.ap.listing.dao.repository.WebsiteRepository;
import com.ap.listing.enums.WebsitePublishingStatus;
import com.ap.listing.model.Scheduler;
import com.ap.listing.model.WebsiteData;
import com.ap.listing.model.WebsitePublisher;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.common.message.LeaderAndIsrResponseData;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Optional;

@Component
@RequiredArgsConstructor
@Slf4j
public class WebsitePublisherApprovalSchedulerProcessor {

    private final WebsitePublisherRepository websitePublisherRepository;
    private final WebsiteRepository websiteRepository;
    private final SchedulerRepository schedulerRepository;

    public void process(Scheduler scheduler) {
        log.info("{} >> {}", getClass().getSimpleName(), scheduler.toString());
        Date now = new Date();
        Optional<WebsitePublisher> websitePublisherOptional = websitePublisherRepository.findById(scheduler.getPrimaryId());
        int timesUsed = scheduler.getTimesUsed();
        if (websitePublisherOptional.isEmpty()) {
            log.info("No website publisher found for id {}", scheduler.getPrimaryId());
            return;
        }
        WebsitePublisher websitePublisher = websitePublisherOptional.get();
        String websiteId = websitePublisher.getWebsiteData().getWebsiteId();
        Optional<WebsiteData> websiteDataOptional = websiteRepository.findById(websiteId);
        if (websiteDataOptional.isEmpty()) {
            log.info("No website found for id {}", websiteId);
            return;
        }
        WebsiteData websiteData = websiteDataOptional.get();
        if (websiteData.getIsActive().equalsIgnoreCase(Boolean.FALSE.toString())) {
            log.info("Website publisher is in inactive state");
            List<Scheduler> byPrimaryId = schedulerRepository.findByPrimaryId(websiteData.getWebsiteId());
            if (!CollectionUtils.isEmpty(byPrimaryId)) {
                List<Boolean> list = new ArrayList<>();
                byPrimaryId.forEach(scheduler1 -> list.add(scheduler1.getIsSchedulingDone()));
                boolean contains = list.contains(Boolean.TRUE);
                if (contains) {
                    websitePublisher.setIsActive(Boolean.FALSE.toString());
                    websitePublisher.setWebsitePublishingStatus(WebsitePublishingStatus.REJECTED.name());
                    websitePublisher.setMessage("Website is not in inactive state");
                    websitePublisher.setDateUpdated(now);
                    WebsitePublisher websitePublisherResponse = websitePublisherRepository.save(websitePublisher);
                    log.info("Website publisher successfully updated : {}", websitePublisherResponse.toString());
                    updateYes(scheduler, now);
                } else {
                    if (timesUsed < 4) updateNo(scheduler, now, timesUsed);
                    else {
                        websitePublisher.setIsActive(Boolean.FALSE.toString());
                        websitePublisher.setWebsitePublishingStatus(WebsitePublishingStatus.REJECTED.name());
                        websitePublisher.setMessage("Website is not in inactive state");
                        websitePublisher.setDateUpdated(now);
                        WebsitePublisher websitePublisherResponse = websitePublisherRepository.save(websitePublisher);
                        log.info("Website publisher successfully updated : {}", websitePublisherResponse.toString());
                        updateYes(scheduler, now);
                    }
                }
            } else {
                websitePublisher.setIsActive(Boolean.FALSE.toString());
                websitePublisher.setWebsitePublishingStatus(WebsitePublishingStatus.REJECTED.name());
                websitePublisher.setMessage("Website is not in inactive state");
                websitePublisher.setDateUpdated(now);
                WebsitePublisher websitePublisherResponse = websitePublisherRepository.save(websitePublisher);
                log.info("Website publisher successfully updated : {}", websitePublisherResponse.toString());
            }
            return;
        }
        String userId = websitePublisher.getUserId();
        List<WebsitePublisher> byUserId = websitePublisherRepository.findByUserId(userId);
        if (byUserId.size() < 10) {
            websitePublisher.setIsActive(Boolean.FALSE.toString());
            websitePublisher.setWebsitePublishingStatus(WebsitePublishingStatus.APPROVED.name());
            websitePublisher.setMessage("Website is not in inactive state");
            websitePublisher.setDateUpdated(now);
            WebsitePublisher websitePublisherResponse = websitePublisherRepository.save(websitePublisher);
            log.info("Website publisher successfully updated : {}", websitePublisherResponse.toString());
            updateYes(scheduler, now);
        } else {
            List<Boolean> list = new ArrayList<>();
            byUserId
                    .forEach(data -> {
                        if (data.getWebsitePublishingStatus().equalsIgnoreCase(WebsitePublishingStatus.APPROVED.name())) {
                            list.add(Boolean.TRUE);
                        }
                    });
            if (list.size() < 10) {
                websitePublisher.setIsActive(Boolean.FALSE.toString());
                websitePublisher.setWebsitePublishingStatus(WebsitePublishingStatus.APPROVED.name());
                websitePublisher.setMessage("Website is not in inactive state");
                websitePublisher.setDateUpdated(now);
                WebsitePublisher websitePublisherResponse = websitePublisherRepository.save(websitePublisher);
                log.info("Website publisher successfully updated : {}", websitePublisherResponse.toString());
            } else {
                List<WebsitePublisher> listData = websitePublisherRepository.findByUserId(userId);
                byUserId
                        .forEach(data -> {
                            data.setIsActive(Boolean.TRUE.toString());
                            data.setWebsitePublishingStatus(WebsitePublishingStatus.APPROVED.name());
                            data.setMessage("Website is not in inactive state");
                            data.setDateUpdated(now);
                            log.info("Website publisher successfully updated : {}", data.toString());
                            listData.add(data);
                        });
                List<WebsitePublisher> websitePublishers = websitePublisherRepository.saveAll(listData);
                log.info("Website Publishers updated : {}", websitePublishers);
            }
            updateYes(scheduler, now);
        }
    }

    private void updateNo(Scheduler scheduler, Date now, int timesUsed) {
        scheduler.setUpdatedOn(now);
        scheduler.setTimesUsed(timesUsed + 1);
        Scheduler schedulerResponse = schedulerRepository.save(scheduler);
        log.info("Scheduler successfully updated : {}", schedulerResponse.toString());
    }

    private void updateYes(Scheduler scheduler, Date now) {
        scheduler.setUpdatedOn(now);
        scheduler.setIsSchedulingDone(true);
        scheduler.setScheduleCompletedOn(now);
        Scheduler schedulerResponse = schedulerRepository.save(scheduler);
        log.info("Scheduler successfully updated : {}", schedulerResponse.toString());
    }
}
