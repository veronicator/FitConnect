package it.unipi.dsmt.FitConnect.entities;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.DocumentReference;

import java.time.LocalDateTime;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Document(collection = "messages")
public class Message {
    @Id
    private String id;
    @DocumentReference(collection = "courses")
    private Course course;
    @DocumentReference(collection = "users")
    private MongoUser sender;
    private LocalDateTime sendTime;
    private String text;

    public Message (Course course, MongoUser sender, LocalDateTime sendTime, String text) {
        this.course = course;
        this.sender = sender;
        this.sendTime = sendTime;
        this.text = text;
    }

    public Message (MongoUser sender, LocalDateTime sendTime, String text) {
        this.sender = sender;
        this.sendTime = sendTime;
        this.text = text;
    }

    public Message (Course course, MongoUser sender, String text) {
        this(course, sender, LocalDateTime.now(), text);
    }

    public Message (MongoUser sender, String text) {
        this(sender, LocalDateTime.now(), text);
    }

}
