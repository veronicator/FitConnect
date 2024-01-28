package it.unipi.dsmt.FitConnect.services;

import it.unipi.dsmt.FitConnect.repositories.mongo.MongoUserRepository;
import it.unipi.dsmt.FitConnect.repositories.mongo.ScheduleRepository;
import it.unipi.dsmt.FitConnect.repositories.mongo.CourseRepository;
import lombok.Getter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Getter
@Service
public class DBService {
    @Autowired
    private MongoUserRepository userRepository;
    @Autowired
    private CourseRepository courseRepository;
    @Autowired
    private ScheduleRepository scheduleRepository;

}
