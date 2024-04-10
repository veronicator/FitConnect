package it.unipi.dsmt.fitconnect.services;

import it.unipi.dsmt.fitconnect.entities.*;
import lombok.AllArgsConstructor;
import lombok.NoArgsConstructor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.*;

@NoArgsConstructor
@AllArgsConstructor
@Service
public class NodeMessageService {
    @Autowired
    private DBService dbService;
    @Autowired
    private RestService restService;

    public String postCourseNotification(CourseNotification courseNotification, String username) {
        LocalDateTime classDateTime = LocalDateTime.ofInstant(
                Instant.ofEpochMilli(
                        Long.parseLong(courseNotification.getTime())), ZoneId.systemDefault());

        String msgToSend = String.format("class booked will start at %s on %s",
                LocalTime.from(classDateTime),
                LocalDate.from(classDateTime));

        return restService.postNotification(username, msgToSend);
    }

    /** method to send a notification message to a specific user through a rest service
     * @param userNotification notification to send
     * @param username username of the user to which send the notification
     * @return a string containing the result of the post method of the rest service*/
    public String postUserNotification(UserNotification userNotification, String username) {
        String courseIdString = userNotification.getCourse().replace("\"", "");
        Course course = dbService.getCourse(courseIdString);
        if (course == null) {
            System.out.println("postUser failed: course not found");
            return "errorPost";
        }

        System.out.println("DEBUG: NodeMessageService - postUserNotification");
        // es. "alice joined the yoga course"
        String msgToSend = String.format("%s %s the %s course",
                userNotification.getUsername(),
                userNotification.getAction(),
                course.getCourseName());
        return restService.postNotification(username, msgToSend);
    }

}
