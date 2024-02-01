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

@Getter
@Setter
@Document(collection = "courses")
public class Course {

    @Id
    private String id;
    private String courseName;
    private String trainer;
    private Integer fitConnecters;   // number of clients enrolled in this course

    @ReadOnlyProperty   // mantenere riferimento persistento o no?
    @DocumentReference(collection = "schedules", lookup = "{ 'course': ?#{#self._id} }")
    private List<Schedule> schedules;   // id orari delle classi

    @ReadOnlyProperty
    @DocumentReference(collection = "users", lookup = "{ 'course': ?#{#self._id} }")
    private List<MongoUser> enrolledUsers; // id utenti iscritti al corso (generico, non alla classe specifica)
//    private List<Message> chatMessages;

    public Course(String courseName, String trainer) {
        this.courseName = courseName.toUpperCase();
        this.trainer = trainer.toUpperCase();
        fitConnecters = 0;
    }

    @Override
    public String toString() {
        return String.format("course: %s class, trainer: %s", courseName, trainer);
    }
}

