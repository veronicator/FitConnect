package it.unipi.dsmt.FitConnect.entities;

import lombok.Getter;
import lombok.Setter;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.ReadOnlyProperty;
import org.springframework.data.annotation.Version;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.DocumentReference;

import java.util.ArrayList;
import java.util.List;

@Getter
@Setter
@Document(collection = "courses")
public class Course {

    @Id
    private String id;
    @Version
    private Long version;
    private String courseName;
    private String trainer; // id o firstname+lastname ?
    private Integer maxReservablePlaces;
    private List<ClassTime> weekSchedule;

    /*
    @ReadOnlyProperty
    @DocumentReference(collection = "reservations", lookup = "{ 'course': ?#{#self._id} }")
    private List<Reservations> reservations;   // id orari delle classi
     */

    @ReadOnlyProperty
    @DocumentReference(collection = "users", lookup = "{ 'course': ?#{#self._id} }")
    private List<MongoUser> enrolledUsers; // id utenti iscritti al corso (generico, non alla classe specifica)
//    private List<Message> chatMessages;

    public Course (String courseName, String trainer, Integer maxReservablePlaces, List<ClassTime> weekSchedule) {
        this.courseName = courseName.toUpperCase();
        this.trainer = trainer;
        this.maxReservablePlaces = maxReservablePlaces;
        this.weekSchedule = weekSchedule;
    }

    public Course (String courseName, String trainer, Integer maxReservablePlaces) {
        this(courseName, trainer, maxReservablePlaces, new ArrayList<>());
    }

    public Course(String courseName, String trainer) {
        this(courseName, trainer, 15, new ArrayList<>());
    }

    public boolean addNewClass(ClassTime newClass) {
        if (weekSchedule == null)
            weekSchedule = new ArrayList<>();
        return weekSchedule.add(newClass);
    }

    public Integer getNumberOfEnrolledUsers() {
        return enrolledUsers.size();
    }

    @Override
    public String toString() {
        return String.format("course: %s class, trainer: %s", courseName, trainer);
    }
}

