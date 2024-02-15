package it.unipi.dsmt.fitconnect.erlang;

//import java.time.LocalDateTime;

public class CourseNotification {
    private String notificationType;
    private String course;
    private String time;

    public CourseNotification(String notificationType, String course, String time) {
        this.notificationType = notificationType;
        this.course = course;
        this.time = time;
    }

    public String getNotificationType() {
        return notificationType;
    }

    public String getCourse() {
        return course;
    }

    public String gettime() {
        return time;
    }
}

