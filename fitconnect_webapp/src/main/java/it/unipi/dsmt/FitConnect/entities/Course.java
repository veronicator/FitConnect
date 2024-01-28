package it.unipi.dsmt.FitConnect.entities;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.ReadOnlyProperty;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.DocumentReference;

import java.util.List;

@ToString
@Getter
@Setter
@AllArgsConstructor
@Document(collection = "courses")
public class Course {

    @Id
    private String id;
    private String courseName;
    private String trainer;

    // todo: pensarci
//    private Integer fitConnecters;   // numero di client iscritti fino ad ora

    @ReadOnlyProperty   // mantenere riferimento persistento o no?
    @DocumentReference(collection = "schedules", lookup = "{'course':?#{#self._id} }")
    private List<Schedule> schedules;   // id orari delle classi

    @ReadOnlyProperty
    @DocumentReference(collection = "users", lookup = "{'course':?#{#self._id} }")
    private List<MongoUser> enrolledUsers; // id utenti iscritti al corso (generico, non alla classe specifica)
//    private List<Message> chatMessages;

    public Course(String courseName, String trainer) {
        this.courseName = courseName;
        this.trainer = trainer;
    }

}

