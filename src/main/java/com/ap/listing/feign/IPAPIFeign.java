/**
 * Proprietary License Agreement
 * <p>
 * Copyright (c) 2025 Bloggios
 * <p>
 * This software is the confidential and proprietary property of Bloggios and is provided under a
 * license, not sold. The application owner is Rohit Parihar and Bloggios. Only authorized
 * Bloggios administrators are permitted to copy, modify, distribute, or sublicense this software
 * under the terms set forth in this agreement.
 * <p>
 * Restrictions
 *
 * You are expressly prohibited from:
 * 1. Copying, modifying, distributing, or sublicensing this software without the express
 *    written permission of Rohit Parihar or Bloggios.
 * 2. Reverse engineering, decompiling, disassembling, or otherwise attempting to derive
 *    the source code of the software.
 * 3. Altering or modifying the terms of this license without prior written approval from
 *    Rohit Parihar and Bloggios administrators.
 * <p>
 * Disclaimer of Warranties:
 * This software is provided "as is" without any warranties, express or implied. Bloggios makes
 * no representations or warranties regarding the software, including but not limited to any
 * warranties of merchantability, fitness for a particular purpose, or non-infringement.
 * <p>
 * For inquiries regarding licensing, please contact: support@bloggios.com.
 */
package com.ap.listing.feign;

/*
  Developer: Rohit Parihar
  Project: bloggios-auth-server
  GitHub: github.com/rohit-zip
  File: IPAPIFeign
 */

import com.ap.listing.payload.response.IPResponse;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;

@FeignClient(
        name = "${feign-client.ipClient.name}",
        url = "${feign-client.ipClient.url}"
)
public interface IPAPIFeign {

    @GetMapping("/{ip}")
    IPResponse getDetails(@PathVariable String ip);
}
