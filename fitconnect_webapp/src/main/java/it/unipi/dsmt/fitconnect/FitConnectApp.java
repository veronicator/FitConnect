package it.unipi.dsmt.fitconnect;

import it.unipi.dsmt.fitconnect.services.RestService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.data.mongodb.repository.config.EnableMongoRepositories;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.transaction.annotation.EnableTransactionManagement;

@EnableMongoRepositories
@EnableScheduling
@EnableTransactionManagement
@SpringBootApplication
public class FitConnectApp {

    @Autowired
    private RestService restService;

    public static void main(String[] args) {
        SpringApplication.run(FitConnectApp.class, args);
    }

}