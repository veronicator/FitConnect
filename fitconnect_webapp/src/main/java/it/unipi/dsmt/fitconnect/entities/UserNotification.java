package it.unipi.dsmt.fitconnect.entities;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
public class UserNotification {
    private String action;  // userJoin or userExited
    private String course;
    private String username;

    public String toString() {
        StringBuilder builtString = new StringBuilder();
        builtString.append("Notification:" + username + " has " + action + " course " + course + "\n");
        return builtString.toString();
    }
}


