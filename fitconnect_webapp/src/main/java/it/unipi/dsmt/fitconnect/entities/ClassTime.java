package it.unipi.dsmt.fitconnect.entities;

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
    protected LocalTime startTime;      // es. 17:00 (on Mongo it's saved as a complete DateTime
    protected LocalTime endTime;        // es. 18:30

    @Override
    public boolean equals(Object obj) {
        if (obj == this)
            return true;
        if (!(obj instanceof ClassTime ct))
            return false;
        return ct.dayOfWeek.equals(this.dayOfWeek)
                && ct.startTime.toString().equals(this.startTime.toString());
    }
}
