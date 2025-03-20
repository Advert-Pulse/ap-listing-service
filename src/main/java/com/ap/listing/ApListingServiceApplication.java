package com.ap.listing;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

@SpringBootApplication(
		scanBasePackages = {
				"com.ap.listing.*",
				"com.bloggios.*"
		}
)
public class ApListingServiceApplication {

	public static void main(String[] args) {
		SpringApplication.run(ApListingServiceApplication.class, args);
	}

}
