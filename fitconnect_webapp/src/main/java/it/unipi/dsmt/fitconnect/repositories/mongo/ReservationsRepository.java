package it.unipi.dsmt.fitconnect.repositories.mongo;

import it.unipi.dsmt.fitconnect.entities.Reservations;
import org.bson.types.ObjectId;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.data.mongodb.repository.Query;
import org.springframework.stereotype.Repository;

import java.time.DayOfWeek;
import java.time.LocalDateTime;
import java.util.List;

@Repository
public interface ReservationsRepository extends MongoRepository<Reservations, String> {
    // todo: aggiungere metodi necessari

    List<Reservations> findByCourse(ObjectId courseId);

    @Query("{ 'bookedUsers': { $regex: ?0, $options: 'i'} }")
    List<Reservations> findByBookedUser(String username);

    @Query(value = "{ 'course': ?0, 'dayOfWeek': ?1, 'startTime': ?2 }", sort = "{'actualClassTime': 1}")
    List<Reservations> findByCourseDayTime(ObjectId course, DayOfWeek day, String startTime);
    //{ '$gt': ?2 }

    @Query(value = "{'dateTime': { '$lt': ?0 } }")
    List<Reservations> findPastReservations(LocalDateTime dateTime);

    @Query(value = "{'course': ?0, 'dayOfWeek': ?1, 'startTime': ?2 }", sort = "{'actualClassTime': 1}", exists = true)
    boolean existsByCourseDayTime(ObjectId course, DayOfWeek day, String startTime);

    @Query(value = "{'dateTime': { '$lt': ?0 } }", delete = true)
    void deletePastReservations(LocalDateTime dateTime);

    void deleteByCourse(ObjectId course);

}
