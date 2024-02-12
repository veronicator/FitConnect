package it.unipi.dsmt.fitconnect.services;

import it.unipi.dsmt.fitconnect.entities.Course;
import it.unipi.dsmt.fitconnect.entities.MongoUser;
import it.unipi.dsmt.fitconnect.repositories.mongo.CourseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.DayOfWeek;
import java.util.List;

@Service
public class CourseService {

    @Autowired
    private CourseRepository courseRepository;

    public Course createNewCourse(String courseName, String trainer) {
        if (courseRepository.existsByCourseNameAndTrainer(courseName, trainer)) {
            System.out.println("Course with this trainer already exists");
            return null;
        }
        return courseRepository.insert(new Course(courseName, trainer));
    }

    public List<Course> findAllCourses() {
        return courseRepository.findAll();
    }

    public List<Course> findCourses(String courseName) {
        return courseRepository.findByCourseName(courseName);
    }

    public List<Course> findCourseByTrainer(String trainer) {
        return courseRepository.findByTrainerUsername(trainer);
    }

    public List<Course> findCoursesByDay(DayOfWeek weekday) {
        return courseRepository.findByDay(weekday);
    }


}
