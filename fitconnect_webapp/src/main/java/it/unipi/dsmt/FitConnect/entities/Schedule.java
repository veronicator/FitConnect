package it.unipi.dsmt.FitConnect.entities;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.Version;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.DocumentReference;
import org.springframework.data.mongodb.core.mapping.Field;

import java.time.*;
import java.time.temporal.ChronoUnit;
import java.time.temporal.TemporalAdjusters;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

@Getter
@Setter
@AllArgsConstructor
@Document(collection = "schedules")
public class Schedule {

    @Id
    private String id;
    @Version
    private Long version;   // for concurrency with OptimisticLocking
    @DocumentReference(collection = "courses", lazy = true)     // lazy: to delay the retrieval of the course until first access of the property
    private Course course;      // courseId
    private boolean periodicClass;  // if not periodic (a special class) the updateSchedule will delete the schedule at all
        // otherwise, the classTime will update with an offset of 7 days

    private LocalDateTime actualClassTime;  // actual timestamp of starting time (to update every week)
    private DayOfWeek dayOfTheWeek;    // es. MONDAY (DayOfWeek.MONDAY)
    private String startTime;      // es. 17:00
    private String endTime;        // es. 18:30
    private Integer maxPlaces;    // max places or to update at every reservation/deletion ?

//    @Field("users")
    @DocumentReference(collection = "users", lookup = "{ 'username': ?#{#target} }")
    private List<MongoUser> bookedUsers;    // todo: testare

    public Schedule(Course course, DayOfWeek dayOfTheWeek, String startTime, String endTime, Integer places) {
        this(course, dayOfTheWeek, startTime, endTime, places, true);
    }

    public Schedule(Course course, DayOfWeek dayOfTheWeek, String startTime, String endTime, Integer places, boolean periodic) {
        this.course = course;
        this.dayOfTheWeek = dayOfTheWeek;
        this.startTime = startTime;
        this.endTime = endTime;
        this.maxPlaces = places;
        this.periodicClass = periodic;
        setActualClassTime();
    }

    private void setActualClassTime() {
        LocalDate nextWeekSchedule = LocalDate.now().with(TemporalAdjusters.next(dayOfTheWeek));
        int[] hh_mm = Arrays.stream(startTime.split("[:.]")).mapToInt(Integer::parseInt).toArray();
        actualClassTime = nextWeekSchedule.atTime(hh_mm[0], hh_mm[1]);
    }

    public String getActualClassDate() {
        return LocalDate.from(actualClassTime).toString();
    }

    public Integer getAvailablePlaces() {
        return (maxPlaces - bookedUsers.size());
    }

    public boolean addBooking(MongoUser user) {
        if (bookedUsers == null)
            bookedUsers = new ArrayList<>();
        return bookedUsers.add(user);
    }

    public boolean isBooked(MongoUser user) {
        return bookedUsers.contains(user);
    }

    public void clearBookingList() {
        bookedUsers.clear();
    }

    // todo: forse modificare valore di ritorno e gestire la periodicità nel db service
    public boolean updateScheduleClass() {
        if (!periodicClass)
            return false;
        bookedUsers.clear();
        actualClassTime = actualClassTime.plus(7, ChronoUnit.DAYS);
        return true;
    }

    @Override
    public String toString() {
        return String.format("{%s: classTimes %s -> '%s', places: %d}",
                dayOfTheWeek, startTime, endTime, maxPlaces);
    }
}
