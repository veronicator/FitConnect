package it.unipi.dsmt.fitconnect.entities;

//import java.time.LocalDateTime;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@AllArgsConstructor
public class CourseNotification {
    private String notificationType;    //expired or modified
    private String course;
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
