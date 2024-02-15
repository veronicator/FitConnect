package it.unipi.dsmt.fitconnect.erlang;

import java.time.LocalDateTime;

public class Message {
    private String course;
    private String username;
    private String text;
    private LocalDateTime time;

    public Message(String course, String username, String text, LocalDateTime time){
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
        StringBuilder builtString = new StringBuilder();
        builtString.append("Course: " + course + "\n");
        builtString.append("Sender: " + username + "\n");
        builtString.append("Content: " + text + "\n");
        builtString.append("Timestamp: " + time + "\n");
        return builtString.toString();
    }
}
