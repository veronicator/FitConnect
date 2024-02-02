package it.unipi.dsmt.FitConnect.entities;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.Version;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.DocumentReference;

import java.time.*;
import java.time.temporal.TemporalAdjusters;
import java.util.ArrayList;
import java.util.List;

@Getter
@Setter
@AllArgsConstructor
@Document(collection = "reservations")
public class Reservations extends ClassTime {

    @Id
    private String id;
    @Version
    private Long version;   // for concurrency with OptimisticLocking
    @DocumentReference(collection = "courses", lazy = true)     // lazy: to delay the retrieval of the course until first access of the property
    private Course course;      // courseId

    private LocalDateTime actualClassTime;  // actual timestamp of starting time
    private Integer reservablePlaces;  // max places or to update at every reservation/deletion ?

    @DocumentReference(collection = "users", lookup = "{ 'username': ?#{#target} }")
    private List<MongoUser> bookedUsers;    // todo: testare

    public Reservations(Course course, DayOfWeek weekDay, LocalTime startTime, LocalTime endTime, Integer places) {
        super(weekDay, startTime, endTime);
        this.course = course;
        this.reservablePlaces = places;
        this.bookedUsers = new ArrayList<>();
        setActualClassTime();
    }

    private void setActualClassTime() {
        LocalDate nextWeekClass = LocalDate.now().with(TemporalAdjusters.next(dayOfWeek));
        actualClassTime = nextWeekClass.atTime(startTime);
    }

    public boolean addBooking(MongoUser user) {
        if (bookedUsers == null)
            bookedUsers = new ArrayList<>();
        if (reservablePlaces <= 0)
            return false;
        reservablePlaces--;
        return bookedUsers.add(user);
    }

    public boolean isBooked(MongoUser user) {
        return bookedUsers.contains(user);
    }

    @Override
    public String toString() {
        return String.format("{%s: classTimes %s -> '%s', places: %d}",
                actualClassTime.getDayOfWeek(), startTime, endTime, reservablePlaces);
    }
}
