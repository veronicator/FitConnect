package it.unipi.dsmt.FitConnect.repositories.mongo;

import it.unipi.dsmt.FitConnect.entities.Reservations;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.data.mongodb.repository.Query;

import java.time.DayOfWeek;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.List;
import java.util.Optional;

public interface ReservationsRepository extends MongoRepository<Reservations, String> {
    // todo: aggiungere metodi necessari

    List<Reservations> findByCourse(String courseId);   // orari di un singolo corso
    @Query("{'bookedUsers.username': ?0")   // to test
    List<Reservations> findByUser(String username);

    @Query("{'course': ?0, 'dayOfWeek': ?1, 'startTime': ?2")
    List<Reservations> findByCourseDayTime(String course, DayOfWeek day, LocalTime startTime);
    //{ '$gt': ?2 }

    @Query(value = "{'dateTime': { '$lt': ?0 } }")
    List<Reservations> findPastReservations(LocalDateTime dateTime);

    @Query(value = "{'dateTime': { '$lt': ?0 } }", delete = true)
    void deletePastReservations(LocalDateTime dateTime);

}
