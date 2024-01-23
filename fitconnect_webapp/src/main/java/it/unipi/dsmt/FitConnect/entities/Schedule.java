package it.unipi.dsmt.FitConnect.entities;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class Schedule {

    private String dayOfTheWeek;
    private String start_time;
    private String end_time;
//    private List<ClassTime> classTimes;  // start_time, end_time of each class of the day "hh:mm, hh:mm"
    private Integer places;

    public Schedule(String dayOfTheWeek, String start_time, String end_time, Integer places) {
        this.dayOfTheWeek = dayOfTheWeek;
        this.start_time = start_time;
        this.end_time = end_time;
//        this.classTimes = classTimes;
        this.places = places;
    }

    @Override
    public String toString() {
        return String.format("{%s: classTimes %s, places:%d}", dayOfTheWeek, start_time, end_time, places);
    }
}
