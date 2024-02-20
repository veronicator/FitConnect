package it.unipi.dsmt.fitconnect.entities;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.bson.types.ObjectId;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.Version;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.DocumentReference;

import java.time.*;
import java.time.temporal.TemporalAdjusters;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Document(collection = "reservations")
public class Reservations {

    @Id
    private ObjectId id;
    @Version
    private Long version;   // for concurrency with OptimisticLocking

    @DocumentReference(collection = "courses", lazy = true)     // lazy: to delay the retrieval of the course until first access of the property
    private Course course;      // stored only the courseId

    private DayOfWeek dayOfWeek;    // es. MONDAY (DayOfWeek.MONDAY)
    private String startTime;      // es. 17:00
    private String endTime;      // es. 18:00
    private LocalDateTime actualClassTime;  // actual timestamp of starting time
    private Integer reservablePlaces;  // max places or to update at every reservation/deletion ?

    @DocumentReference(collection = "users", lookup = "{ 'username': ?#{#target} }")
    private List<MongoUser> bookedUsers;

    public Reservations(Course course, DayOfWeek weekDay, String startTime, String endTime, Integer places) {
        this.dayOfWeek = weekDay;
        this.startTime = startTime;
        this.endTime = endTime;
        this.course = course;
        this.reservablePlaces = places;
        this.bookedUsers = new ArrayList<>();
        setActualClassTime();
    }

    public Reservations(Course course, LocalDateTime actualClassTime,
                        DayOfWeek weekDay, String startTime, String endTime, Integer places) {
        this.dayOfWeek = weekDay;
        this.startTime = startTime;
        this.endTime = endTime;
        this.course = course;
        this.reservablePlaces = places;
        this.actualClassTime = actualClassTime;
    }

    public Reservations(Course course, LocalDateTime actualClassTime,
                        DayOfWeek weekDay, LocalTime startTime, LocalTime endTime, Integer places) {
        this(course, actualClassTime, weekDay, startTime.toString(), endTime.toString(), places);
    }

    public Reservations(Course course, DayOfWeek weekDay, LocalTime startTime, LocalTime endTime, Integer places) {
        this(course, weekDay, startTime.toString(), endTime.toString(), places);

    }

    private void setActualClassTime() {
        LocalDate nextWeekClass = LocalDate.now().with(TemporalAdjusters.next(dayOfWeek));
//        actualClassTime = nextWeekClass.atTime(startTime);
        int[] split = Arrays.stream(startTime.split("[:.]")).mapToInt(Integer::parseInt).toArray();
        actualClassTime = nextWeekClass.atTime(split[0], split[1]);
    }

    public boolean addBooking(MongoUser user) {
        if (bookedUsers == null)
            bookedUsers = new ArrayList<>();
        if (reservablePlaces <= 0 || isBooked(user))
            return false;
        reservablePlaces--;
        return bookedUsers.add(user);
    }

    public boolean removeBooking(MongoUser user) {
        if (bookedUsers == null)
            return false;
        if (bookedUsers.remove(user)) {
            reservablePlaces--;
            return true;
        }
        return false;
    }

    public LocalDate getClassDate() {
        return LocalDate.from(actualClassTime);
    }

    public Integer getMaxPlaces() {
//        return (bookedUsers.size() + reservablePlaces);
        return course.getMaxReservablePlaces();
    }

    public boolean isBooked(MongoUser user) {
        if (bookedUsers == null)
            return false;
        return bookedUsers.contains(user);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this)
            return true;
        if (!(obj instanceof Reservations r))
            return false;
        return r.id.equals(this.id);
    }

    @Override
    public String toString() {
        return String.format("{%s: classTimes %s -> %s, places: %d}",
                actualClassTime.getDayOfWeek(), startTime, endTime, reservablePlaces);
    }
}
