package it.unipi.dsmt.FitConnect.repositories.mongo;

import it.unipi.dsmt.FitConnect.entities.Schedule;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.data.mongodb.repository.Query;

import java.util.List;

public interface ScheduleRepository extends MongoRepository<Schedule, String> {
    // todo: aggiungere metodi necessari

    List<Schedule> findByCourse(String courseId);   // orari di un singolo corso
    @Query("{'bookedUsers.username': ?0")   // to test
    List<Schedule> findByUser(String username);

}
