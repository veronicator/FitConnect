package it.unipi.dsmt.fitconnect.entities;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Setter
@Getter
@AllArgsConstructor
@NoArgsConstructor
public class Notification {
    private String action;
    private String courseId;  // it's the course id, to cast to ObjectId when interact with MongoDb
//    private String courseName;
//    private String trainer;     // username or complete name
    private String username;

    @Override
    public String toString() {
        return "Notification{" +
                "action='" + action + '\'' +
                ", courseId='" + courseId + '\'' +
//                ", courseName='" + courseName + '\'' +
//                ", trainer='" + trainer + '\'' +
                ", username='" + username + '\'' +
                '}';
    }
}
