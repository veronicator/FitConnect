package it.unipi.dsmt.fitconnect.entities;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.bson.types.ObjectId;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.ReadOnlyProperty;
import org.springframework.data.annotation.Version;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.DocumentReference;

import java.util.ArrayList;
import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Document(collection = "courses")
public class Course {

    @Id
    private ObjectId id;
    @Version
    private Long version;
    private String courseName;
    private String trainer; // firstname+lastname
    private String trainerUsername; // or id ?
    private Integer maxReservablePlaces;
    private List<ClassTime> weekSchedule;

    @ReadOnlyProperty
    @DocumentReference(collection = "users", lookup = "{ 'courses': ?#{#self._id} }")
    private List<MongoUser> enrolledClients; // id utenti iscritti al corso (generico, non alla classe specifica)

    public Course (String courseName, String trainer, Integer maxReservablePlaces, List<ClassTime> weekSchedule) {
        this.courseName = courseName;
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

    public boolean classAlreadyScheduled(ClassTime classTime) {
        return weekSchedule.contains(classTime);
    }

    public Integer getNumberOfEnrolledUsers() {
        return enrolledClients.size();
    }

    @Override
    public String toString() {
        return String.format("course: %s, trainer: %s", courseName, trainer);
    }
}

