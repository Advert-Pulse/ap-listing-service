package com.ap.listing.utils;

/*
  Developer: Rohit Parihar
  Project: ap-listing-service
  GitHub: github.com/rohit-zip
  File: LoggingAspect
 */

import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Aspect
@Component
public class AuditLog {

    private static final Logger logger = LoggerFactory.getLogger(AuditLog.class);

    @Before("execution(* com.ap.listing.service.implementation.*.*(..))" +
            "||" +
            "execution(* com.ap.listing.transformer.*.*(..))")
    public void logAfterMethod(JoinPoint joinPoint) {
        String className = joinPoint.getTarget().getClass().getSimpleName();
        String methodName = joinPoint.getSignature().getName();
        Object[] args = joinPoint.getArgs();
        logger.info("Audit Log: {} >> {} -> args: {}", className, methodName, args);
    }
}
