package it.unipi.dsmt.FitConnect.services;

import it.unipi.dsmt.FitConnect.entities.*;
import it.unipi.dsmt.FitConnect.repositories.mongo.*;
import lombok.Getter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.OptimisticLockingFailureException;
import org.springframework.stereotype.Service;

import java.time.DayOfWeek;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.List;
import java.util.Optional;

@Getter
@Service
public class DBService {
    @Autowired
    private MongoUserRepository userRepository;
    @Autowired
    private CourseRepository courseRepository;
    @Autowired
    private ReservationsRepository reservationsRepository;

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

    public boolean addClassTime(String courseId, DayOfWeek dayOfWeek, LocalTime startTime, LocalTime endTime) {
        try {
            Optional<Course> optCourse = courseRepository.findById(courseId);
            if (optCourse.isEmpty()) {
                System.out.println("addClassTime failed: course not found");
                return false;
            }
            Course course = optCourse.get();
            // todo: check class time
            if (course.addNewClass(new ClassTime(dayOfWeek, startTime, endTime))) {
                courseRepository.save(course);
            }
        } catch (OptimisticLockingFailureException e) {
            return false;
        }
        return true;
    }

    public boolean bookClass(String username, String course, DayOfWeek dayOfWeek, LocalTime startTime) {
        try {
            Optional<MongoUser> user = userRepository.findByUsername(username);
            if (user.isEmpty()) {
                System.out.println("Booking failed: user not found.");
                return false;
            }

            List<Reservations> availableClass = reservationsRepository.findByCourseDayTime(
                    course, dayOfWeek, startTime);
            if (availableClass.isEmpty()) {
                System.out.println("Booking failed: no available courses found");
                return false;
            }
            Reservations reservations = availableClass.getFirst();
            if (reservations.getActualClassTime().isBefore(LocalDateTime.now())) {
                System.out.println("Booking not possible: class already held this week");
                return false;
            }
            if (reservations.addBooking(user.get())) {
                reservations = reservationsRepository.save(reservations);
                System.out.println("class booking made for the day " + reservations.getClassDate());
            }

        } catch (OptimisticLockingFailureException e) {
            System.out.println("Booking failed");
            return false;
        }
        return true;
    }

}
