package it.unipi.dsmt.FitConnect.erlang;

public class UserNotification {
    private String action;
    private String course;
    private String username;

    public UserNotification(String action, String course, String username) {
        this.action = action;
        this.course = course;
        this.username = username;
    }

    public String getAction() {
        return action;
    }

    public String getCourse() {
        return course;
    }

    public String getUsername() {
        return username;
    }

    public String toString() {
        StringBuilder builtString = new StringBuilder();
        builtString.append("Notification:" + username + " has " + action + " course " + course + "\n");
        return builtString.toString();
    }
}


