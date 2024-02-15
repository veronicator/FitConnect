package it.unipi.dsmt.fitconnect.entities;

import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.DocumentReference;

import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
//@ToString
@Document(collection = "messages")
public class Message {
    @Id
    private String id;
    @DocumentReference(collection = "courses")
    private Course course;
    @DocumentReference(collection = "users", lookup = "{ 'username': ?#{#target} }")
    private MongoUser sender;
    private LocalDateTime sendTime;
    private String text;

    public Message (Course course, MongoUser sender, LocalDateTime sendTime, String text) {
        this.course = course;
        this.sender = sender;
        this.sendTime = sendTime;
        this.text = text;
    }

    public Message (Course course, MongoUser sender, String text) {
        this(course, sender, LocalDateTime.now(), text);
    }

    @Override
    public String toString() {
        return "Message{" +
                " course=" + course.getCourseName() +
                ", sender=" + sender.getUsername() +
                ", sendTime=" + sendTime.truncatedTo(ChronoUnit.MINUTES) +
                ", text='" + text + '\'' +
                '}';
    }
}
