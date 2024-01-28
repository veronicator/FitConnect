package it.unipi.dsmt.FitConnect.services;

import it.unipi.dsmt.FitConnect.entities.Course;
import it.unipi.dsmt.FitConnect.entities.User;
import it.unipi.dsmt.FitConnect.repositories.CourseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

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
        return courseRepository.findByTrainer(trainer);
    }

    public List<Course> findCoursesByDay(String weekday) {
        return courseRepository.findByDay(weekday);
    }

    public List<Course> findUserCourses(String username) {
        return courseRepository.findByUser(username);
    }

    public void subscribe(String course, User user) {
        courseRepository.updateEnrolledList(course, user);
    }

    public void removeSubscription(String course, String email) {   // todo: sostituire con username
        courseRepository.removeSubscription(course, email);
    }
}
