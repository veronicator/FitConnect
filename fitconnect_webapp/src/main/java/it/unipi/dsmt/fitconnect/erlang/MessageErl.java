package it.unipi.dsmt.fitconnect.erlang;

import java.time.LocalDateTime;

public class MessageErl {
    private String course;
    private String username;
    private String text;
    private LocalDateTime time;

    public MessageErl(String course, String username, String text, LocalDateTime time){
        this.course = course;
        this.username = username;
        this.text = text;
        this.time = time;
    }

    public String getCourse() {
        return course;
    }

    public String getUsername() {
        return username;
    }

    public String getText() {
        return text;
    }

    public LocalDateTime getTime() {
        return time;
    }

    @Override
    public String toString() {
        String builtString = "Course: " + course + "\n" +
                "Sender: " + username + "\n" +
                "Content: " + text + "\n" +
                "Timestamp: " + time + "\n";
        return builtString;
    }
}