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
package com.ap.listing.service.implementation;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: TaskServiceImplementation
 */

import com.ap.listing.constants.ServiceConstants;
import com.ap.listing.dao.repository.TaskBuyerRepository;
import com.ap.listing.dao.repository.TaskPublisherRepository;
import com.ap.listing.enums.ErrorData;
import com.ap.listing.exception.BadRequestException;
import com.ap.listing.model.TaskBuyer;
import com.ap.listing.model.TaskPublisher;
import com.ap.listing.payload.response.*;
import com.ap.listing.properties.TaskBuyerListProperties;
import com.ap.listing.properties.TaskPublisherListProperties;
import com.ap.listing.service.TaskService;
import com.ap.listing.transformer.TaskToDetailedTaskTransformer;
import com.ap.listing.transformer.TaskBuyerToTaskBuyerResponseTransformer;
import com.ap.listing.transformer.TaskPublisherToTaskPublisherResponseTransformer;
import com.bloggios.query.payload.ListPayload;
import com.bloggios.query.processor.ListProcessor;
import com.bloggios.query.query.InitQuery;
import jakarta.persistence.TypedQuery;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Set;

@Service
@RequiredArgsConstructor
@Slf4j
public class TaskServiceImplementation implements TaskService {

    private final ListProcessor listProcessor;
    private final TaskBuyerListProperties taskBuyerListProperties;
    private final TaskPublisherListProperties taskPublisherListProperties;
    private final InitQuery<TaskBuyer> buyerInitQuery;
    private final TaskBuyerToTaskBuyerResponseTransformer taskBuyerToTaskBuyerResponseTransformer;
    private final InitQuery<TaskPublisher> publisherInitQuery;
    private final TaskPublisherToTaskPublisherResponseTransformer taskPublisherToTaskPublisherResponseTransformer;
    private final TaskBuyerRepository taskBuyerRepository;
    private final TaskToDetailedTaskTransformer taskToDetailedTaskTransformer;
    private final TaskPublisherRepository taskPublisherRepository;

    @Override
    public ResponseEntity<ListResponse> listBuyerTasks(ListPayload listPayload) {
        ListPayload transformedListPayload = listProcessor.initProcess(listPayload, taskBuyerListProperties.getData(), "dateUpdated");
        TypedQuery<TaskBuyer> build = buyerInitQuery.build(transformedListPayload, TaskBuyer.class);
        List<TaskBuyerResponse> taskBuyerResponses = build
                .getResultList()
                .stream()
                .map(taskBuyerToTaskBuyerResponseTransformer::transform)
                .toList();
        ListResponse listResponse = ListResponse
                .builder()
                .object(taskBuyerResponses)
                .page(listPayload.getPage())
                .size(listPayload.getSize())
                .totalRecordsCount(buyerInitQuery.getTotalRecords(transformedListPayload, TaskBuyer.class))
                .build();
        return ResponseEntity.ok(listResponse);
    }

    @Override
    public ResponseEntity<ListResponse> listPublisherTasks(ListPayload listPayload) {
        ListPayload transformedListPayload = listProcessor.initProcess(listPayload, taskPublisherListProperties.getData(), "dateUpdated");
        TypedQuery<TaskPublisher> build = publisherInitQuery.build(transformedListPayload, TaskPublisher.class);
        List<TaskPublisherResponse> taskPublisherResponses = build
                .getResultList()
                .stream()
                .map(taskPublisherToTaskPublisherResponseTransformer::transform)
                .toList();
        ListResponse listResponse = ListResponse
                .builder()
                .object(taskPublisherResponses)
                .page(listPayload.getPage())
                .size(listPayload.getSize())
                .totalRecordsCount(publisherInitQuery.getTotalRecords(transformedListPayload, TaskPublisher.class))
                .build();
        return ResponseEntity.ok(listResponse);
    }

    @Override
    public ResponseEntity<ListResponse> myListBuyerTasks(ListPayload listPayload) {
        return null;
    }

    @Override
    public ResponseEntity<ListResponse> myListPublisherTasks(ListPayload listPayload) {
        return null;
    }

    @Override
    public ResponseEntity<DetailedTaskResponse> getTaskDetails(String taskId, String preference) {
        Set<String> validPreferences = Set.of(ServiceConstants.BUYER, ServiceConstants.SELLER);
        if (!validPreferences.contains(preference)) {
            throw new BadRequestException(ErrorData.INVALID_PREFERENCE, "preference");
        }
        DetailedTaskResponse detailedTaskResponse;
        if (preference.equalsIgnoreCase(ServiceConstants.BUYER)) {
            TaskBuyer taskBuyer = taskBuyerRepository.findByTaskId(taskId)
                    .orElseThrow(() -> new BadRequestException(ErrorData.TASK_BUYER_NOT_FOUND));
            detailedTaskResponse = taskToDetailedTaskTransformer.transform(taskBuyer);
        } else {
            TaskPublisher taskPublisher = taskPublisherRepository.findByTaskId(taskId)
                    .orElseThrow(() -> new BadRequestException(ErrorData.TASK_PUBLISHER_NOT_FOUND));
            detailedTaskResponse = taskToDetailedTaskTransformer.transform(taskPublisher);
        }
        return ResponseEntity.ok(detailedTaskResponse);
    }
}
