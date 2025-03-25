package com.ap.listing;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.openfeign.EnableFeignClients;

@SpringBootApplication(
		scanBasePackages = {
				"com.ap.listing.*",
				"com.bloggios.*"
		}
)
@EnableFeignClients
public class ApListingServiceApplication {

	public static void main(String[] args) {
		SpringApplication.run(ApListingServiceApplication.class, args);
	}

}
