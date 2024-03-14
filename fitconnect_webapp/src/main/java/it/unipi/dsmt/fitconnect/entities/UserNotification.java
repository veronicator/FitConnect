package it.unipi.dsmt.fitconnect.entities;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
public class UserNotification {
    private String action;  // userJoined or userExited
    private String course;
    private String username;

    @Override
    public String toString() {
        return "Notification:" + username + " has " + getAction() + " course " + course + "\n";
    }

    public String getAction() {
        if (action.equalsIgnoreCase("userJoined"))
            return "joined";
        return "exited";
    }
}


