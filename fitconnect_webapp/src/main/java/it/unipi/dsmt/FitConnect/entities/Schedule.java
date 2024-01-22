package it.unipi.dsmt.FitConnect.entities;

import lombok.Getter;
import lombok.Setter;

import java.util.List;

@Getter
@Setter
public class Schedule {

    private String dayOfTheWeek;
    private List<ClassTime> classTimes;  // start_time, end_time of each class of the day "hh:mm, hh:mm"
    private Integer places;

    public Schedule(String dayOfTheWeek, List<ClassTime> classTimes, Integer places) {
        this.dayOfTheWeek = dayOfTheWeek;
        this.classTimes = classTimes;
        this.places = places;
    }

    @Override
    public String toString() {
        return String.format("{%s: classTimes %s, places:%d}", dayOfTheWeek, classTimes, places);
    }
}
