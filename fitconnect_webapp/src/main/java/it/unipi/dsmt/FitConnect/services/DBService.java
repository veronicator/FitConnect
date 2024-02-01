package it.unipi.dsmt.FitConnect.services;

import it.unipi.dsmt.FitConnect.entities.*;
import it.unipi.dsmt.FitConnect.repositories.mongo.*;
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

    public boolean addCourse(String courseName, String trainer) {
        try {
            if (courseRepository.existsByCourseNameAndTrainer(courseName, trainer)) {
                System.out.println("course with this trainer already exists");
                return false;
            }

            Course newCourse = new Course(courseName, trainer);
            newCourse = courseRepository.insert(newCourse);
            System.out.println("added new " + newCourse);
        } catch (Exception e) {
            System.out.println("addCourse exception");
            return false;
        }
        return true;
    }

    public boolean addSchedule(Schedule newSchedule) {
        /* do something */
        return true;
    }

}
