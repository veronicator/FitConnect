package it.unipi.dsmt.FitConnect.entities;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class ClassTime {
    String start_time;
    String end_time;

    public ClassTime(String start_time, String end_time) {
        this.start_time = start_time;
        this.end_time = end_time;
    }

    @Override
    public String toString() {
        return String.format("%s -> %s", start_time, end_time);
    }
}
