package it.unipi.dsmt.fitconnect.entities;

import it.unipi.dsmt.fitconnect.enums.CourseType;
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

    private CourseType courseName;
    private String trainer; // firstname lastname
    private String trainerUsername; // it's unique, so it can be used as an identifier
    private Integer maxReservablePlaces;

    private List<ClassTime> weekSchedule;

    @ReadOnlyProperty
    @DocumentReference(collection = "users", lookup = "{ 'courses': ?#{#self._id} }")
    private List<MongoUser> enrolledClients; // id clients enrolled at this course (it will also contain the trainerId)

    public Course (CourseType courseName, String trainerCompleteName, String trainerUsername, Integer maxReservablePlaces) {
        this.courseName = courseName;
        this.trainer = trainerCompleteName;
        this.trainerUsername = trainerUsername;
        this.maxReservablePlaces = maxReservablePlaces;
    }

    public boolean addNewClass(ClassTime newClass) {
        if (weekSchedule == null)
            weekSchedule = new ArrayList<>();
        return weekSchedule.add(newClass);
    }

    public boolean removeClass(ClassTime classToRemove) {
        if (weekSchedule == null)
            return false;
        return weekSchedule.remove(classToRemove);
    }

    public boolean classScheduled(ClassTime classTime) {
        if (weekSchedule == null)
            return false;
        return weekSchedule.contains(classTime);
    }

    public Integer getNumberOfEnrolledUsers() {
        if (enrolledClients == null)
            return 0;
        if (enrolledClients.size() > 0) {
            // also the trainer is considered during the lookup
            return enrolledClients.size() - 1;
        }
        return 0;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this)
            return true;
        if (!(obj instanceof Course c))
            return false;
        return c.id.equals(this.id);
    }

//    @Override
//    public String toString() {
//        return String.format("course: %s class, trainer: %s", courseName, trainer);
//    }


    @Override
    public String toString() {
        return "Course{" +
                "courseName='" + courseName + '\'' +
                ", trainer='" + trainer + '\'' +
                '}';
    }
}
