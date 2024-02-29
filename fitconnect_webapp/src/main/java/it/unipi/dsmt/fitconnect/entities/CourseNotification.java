package it.unipi.dsmt.fitconnect.entities;

//import java.time.LocalDateTime;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
public class CourseNotification {
    private String notificationType;    //expired or edited
    private String course;      // Reservations ID
    private String time;

    @Override
    public String toString() {
        return "CourseNotification{" +
                "notificationType='" + notificationType + '\'' +
                ", course='" + course + '\'' +
                ", time='" + time + '\'' +
                '}';
    }
}

