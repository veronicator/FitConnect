package it.unipi.dsmt.FitConnect.entities;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.Version;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.DocumentReference;

import java.time.Instant;
import java.util.Date;
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

    private String dayOfTheWeek;    // es. monday
    private String start_time;      // es. 17:00
    private String end_time;        // es. 18:30
    private Integer availablePlaces;    // max places or to update at every reservation/deletion ?

    @DocumentReference(collection = "users", lookup = "{ 'username': ?#{#target} }")
    private List<User> bookedUsers;    // todo: testare
//    private List<GeneralUser> enrolled;
    // inserire timestamp con data esatta del corso
//    private Date date = new Date("2024-02-02 17:00");   // todo capire come gestire gli orari dei corsi
//    private Instant timeInstant;  // timestamp di inizio corso


    public Schedule(String dayOfTheWeek, String start_time, String end_time, Integer places) {
        this.dayOfTheWeek = dayOfTheWeek;
        this.start_time = start_time;
        this.end_time = end_time;
        this.availablePlaces = places;
    }

    @Override
    public String toString() {
        return String.format("{%s: classTimes %s -> '%s', places: %d}",
                dayOfTheWeek, start_time, end_time, availablePlaces);
    }
}
