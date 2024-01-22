package it.unipi.dsmt.FitConnect.entities;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

import java.util.List;

@ToString
@Getter
@Setter
@Document(collection = "courses")
public class Course {

    @Id
    private String id;
    private String courseName;
    private String trainer;
    private List<Schedule> schedules;
    private List<GeneralUser> enrolled;

    public Course(String courseName, String trainer, List<Schedule> schedules, List<GeneralUser> enrolled) {
        this.courseName = courseName;
        this.trainer = trainer;
        this.schedules = schedules;
        this.enrolled = enrolled;
    }
}

