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
    private String course;
//    @DocumentReference(collection = "courses")
//    private Course course;
//    @DocumentReference(collection = "users", lookup = "{ 'username': ?#{#target} }")
//    private MongoUser sender;
    private String sender;
    private LocalDateTime sendTime;
    private String text;

    public Message (String course, String sender, String text, LocalDateTime sendTime) {
        this.course = course;
        this.sender = sender;
        this.text = text;
        this.sendTime = sendTime;
    }

    public Message (String course, String sender, String text) {
        this(course, sender, text, LocalDateTime.now());
    }

    @Override
    public String toString() {
        return "Message{" +
                " course=" + course +
                ", sender=" + sender +
                ", sendTime=" + sendTime.truncatedTo(ChronoUnit.MINUTES) +
                ", text='" + text + '\'' +
                '}';
    }
}
