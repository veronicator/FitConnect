package it.unipi.dsmt.fitconnect.entities;

import lombok.*;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class WSMessage {
    private String messageContent;

    public WSMessage(CourseNotification courseNotification) {

    }
}
