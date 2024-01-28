package it.unipi.dsmt.FitConnect.services;

import it.unipi.dsmt.FitConnect.repositories.ScheduleRepository;
import it.unipi.dsmt.FitConnect.repositories.UserRepository;
import it.unipi.dsmt.FitConnect.repositories.mongo.CourseRepository;
import lombok.Getter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Getter
@Service
public class DBService {
    @Autowired
    private UserRepository userRepository;
    @Autowired
    private CourseRepository courseRepository;
    @Autowired
    private ScheduleRepository scheduleRepository;

}
