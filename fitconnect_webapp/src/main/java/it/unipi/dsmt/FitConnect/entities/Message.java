package it.unipi.dsmt.FitConnect.entities;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import org.springframework.data.mongodb.core.mapping.DocumentReference;

import java.time.Instant;

@Getter
@Setter
@AllArgsConstructor
public class Message {
    @DocumentReference(collection = "users")
    private User sender;
    private Instant sendTime;
    private String text;

    public Message (User sender, String text) {
        this(sender, Instant.now(), text);
    }

}
