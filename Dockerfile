# Use the eclipse-temurin:21.0.4_7-jdk-alpine as the base image
FROM eclipse-temurin:21.0.4_7-jdk-alpine AS builder
LABEL maintainer="Team Bloggios"
LABEL developer="Rohit Parihar rohitparih@gmail.com"

ARG ARTIFACTORY_URL
ARG ARTIFACTORY_TOKEN

ENV ARTIFACTORY_URL=${ARTIFACTORY_URL}
ENV ARTIFACTORY_TOKEN=${ARTIFACTORY_TOKEN}

# Set the working directory inside the container
WORKDIR /app

# Copy the build.gradle, settings.gradle, and gradlew files
COPY build.gradle .
COPY settings.gradle .
COPY gradlew .
COPY gradle gradle

# Copy the source code
COPY src src
COPY LICENSE LICENSE

# Build the application
RUN ./gradlew build

FROM eclipse-temurin:21.0.4_7-jre-alpine

# Set the working directory inside the container
WORKDIR /app

# Copy the built JAR file from the builder stage
COPY --from=builder /app/build/libs/*.jar app.jar

# Expose the port that the application will run on
EXPOSE 8080

# Set the entry point to run the application
ENTRYPOINT ["java", "-jar", "app.jar"]