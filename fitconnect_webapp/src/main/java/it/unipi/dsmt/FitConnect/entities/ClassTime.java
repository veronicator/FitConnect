package it.unipi.dsmt.FitConnect.entities;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.time.DayOfWeek;
import java.time.LocalTime;

@Setter
@Getter
@AllArgsConstructor
@NoArgsConstructor
public class ClassTime {
    protected DayOfWeek dayOfWeek;    // es. MONDAY (DayOfWeek.MONDAY)
    protected LocalTime startTime;      // es. 17:00
    protected LocalTime endTime;        // es. 18:30

}