package it.unipi.dsmt.FitConnect.services;

import it.unipi.dsmt.FitConnect.entities.Course;
import it.unipi.dsmt.FitConnect.repositories.CourseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class CourseService {

    @Autowired
    private CourseRepository courseRepository;

    public Course createNewCourse(String courseName, String trainer) {
        // if exists courseName && trainer -> return null else insert new
        return null;
    }
}
