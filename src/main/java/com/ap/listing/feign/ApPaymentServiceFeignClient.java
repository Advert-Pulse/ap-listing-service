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
package com.ap.listing.feign;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: ApPaymentServiceFeignClient
 */

import com.ap.listing.configuration.FeignConfig;
import com.ap.listing.constants.ServiceConstants;
import com.ap.listing.payload.request.ReserveFundToBalanceRequest;
import com.ap.listing.payload.request.ReverseReservedFundsRequest;
import com.ap.listing.payload.request.SendFundsToReservedRequest;
import com.ap.listing.payload.response.WalletResponse;
import com.bloggios.provider.payload.ModuleResponse;
import feign.Headers;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.*;

@FeignClient(
        name = "${feign-client.ap-payment-service.name}",
        url = "${feign-client.ap-payment-service.url}",
        configuration = FeignConfig.class
)
@Headers("Authorization: {token}")
public interface ApPaymentServiceFeignClient {

    @GetMapping("/v1/wallet")
    WalletResponse getWallet(@RequestHeader(ServiceConstants.AUTHORIZATION) String token, @RequestParam String preference);

    @PostMapping("/v1/wallet/send-reserved")
    ModuleResponse sendFundsToReserved(@RequestHeader(ServiceConstants.AUTHORIZATION) String token, @RequestBody SendFundsToReservedRequest sendFundsToReservedRequest);

    @PostMapping("/v1/wallet/reverse-reserved")
    ModuleResponse reverseReservedFund(@RequestHeader(ServiceConstants.AUTHORIZATION) String token, @RequestBody ReverseReservedFundsRequest reverseReservedFundsRequest);

    @PostMapping("/v1/wallet/reserved-to-balance")
    ModuleResponse reserveFundsToBalance(@RequestHeader(ServiceConstants.AUTHORIZATION) String token, @RequestBody ReserveFundToBalanceRequest reserveFundToBalanceRequest);

    @PostMapping("/v1/internal/reverse-reserved")
    ModuleResponse reverseReservedFundInternal(@RequestHeader(ServiceConstants.AP_PAYMENT_INTERNAL_KEY) String internalKey, @RequestBody ReverseReservedFundsRequest reverseReservedFundsRequest);
}
