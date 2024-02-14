package it.unipi.dsmt.fitconnect;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.data.mongodb.repository.config.EnableMongoRepositories;
import org.springframework.scheduling.annotation.EnableScheduling;

@EnableMongoRepositories
@EnableScheduling
@SpringBootApplication
public class FitConnectApp {
    public static void main(String[] args) {
        SpringApplication.run(FitConnectApp.class, args);
    }
}