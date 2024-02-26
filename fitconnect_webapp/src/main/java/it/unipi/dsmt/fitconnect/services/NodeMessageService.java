package it.unipi.dsmt.fitconnect.services;

import it.unipi.dsmt.fitconnect.entities.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class NodeMessageService {
    @Autowired
    private DBService dbService;
    @Autowired
    private RestService restService;

    public String postCourseNotification(CourseNotification courseNotification, String username) {
        Reservations reservations = dbService.getReservations(courseNotification.getCourse());
        if (reservations == null) {
            System.out.println("postCourse failed: reservations not found");
            return "errorPost";
        }
        String msgToSend = String.format("%s class booked will start at %s today",
                reservations.getCourse().getCourseName(),
                courseNotification.getTime());
        return restService.postNotification(username, msgToSend);
    }

    public String postUserNotification(UserNotification userNotification, String username) {
        Course course = dbService.getCourse(userNotification.getCourse());
        if (course == null) {
            System.out.println("postUser failed: course not found");
            return "errorPost";
        }
        // es. "alice joined the yoga course"
        String msgToSend = String.format("%s %s the %s course",
                userNotification.getUsername(),
                userNotification.getAction(),
                course.getCourseName());
        return restService.postNotification(username, msgToSend);
    }

}
