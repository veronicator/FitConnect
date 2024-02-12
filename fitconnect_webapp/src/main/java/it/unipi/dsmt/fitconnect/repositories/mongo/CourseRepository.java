package it.unipi.dsmt.fitconnect.repositories.mongo;

import it.unipi.dsmt.fitconnect.entities.Course;
import it.unipi.dsmt.fitconnect.entities.MongoUser;
import org.bson.types.ObjectId;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.data.mongodb.repository.*;
import org.springframework.stereotype.Repository;

import java.time.DayOfWeek;
import java.util.List;
import java.util.Optional;

@Repository
public interface CourseRepository extends MongoRepository<Course, String> {

    /** find methods */
    //DEFAULT (case-insensitive) {"firstname" : { $regex: firstname, $options: 'i'}}
    Page<Course> findAll(Pageable pageable);
    @Query("{'courseName': { $regex: ?0, $options: 'i'}}")
    List<Course> findByCourseName(String courseName);
    @Query("{'trainerUsername': { $regex: ?0, $options: 'i'}}")
    List<Course> findByTrainerUsername(String trainer);
    @Query("{'weekSchedule.dayOfWeek': ?0}")
    List<Course> findByDay(DayOfWeek dayOfWeek);
    @Query("{'courseName': { $regex: ?0, $options: 'i'}, 'trainer': { $regex: ?1, $options: 'i'} }")
    Optional<Course> findByCourseNameAndTrainer(String course, String trainer);
    @Query("{'courseName': { $regex: ?0, $options: 'i'}, 'trainerUsername': { $regex: ?1, $options: 'i'} }")
    Optional<Course> findByCourseNameAndTrainerUsername(String course, String trainerUsername);


    /** existsBy methods */

    @Query(value = "{'courseName': { $regex: ?0, $options: 'i'}}", exists = true)
    boolean existsByCourseName(String courseName);
    @Query(value = "{'trainer': { $regex: ?0, $options: 'i'}}", exists = true)
    boolean existsByTrainer(String trainerCompleteName);
    @Query(value = "{'trainerUsername': { $regex: ?0, $options: 'i'}}", exists = true)
    boolean existsByTrainerUsername(String trainerUsername);
    @Query(value = "{'courseName': { $regex: ?0, $options: 'i'}, 'trainerUsername': { $regex: ?1, $options: 'i'} }", exists = true)
    boolean existsByCourseNameAndTrainer(String courseName, String trainerUsername);

    /** deleteBy methods */

    @Query(value = "{'trainerUsername': { $regex: ?0, $options: 'i'}}", delete = true)
    void deleteByTrainer(String trainerUsername);

    /** update methods */

    @Query("{'id': ?0 }")
    @Update("{ $set: {'courseName': ?1 }}")
    void updateCourseName(ObjectId courseId, String newName);

    @Query("{'courseName': { $regex: ?0, $options: 'i'} }")
    @Update("{ $set: {'courseName': ?1 }}")
    void updateMultiCourseName(String oldName, String newName);

    @Query("{'trainerUsername': { $regex: ?0, $options: 'i'} }")
    @Update("{ $set: {'trainerUsername': ?1, 'trainer': ?2 } }")
    void updateTrainer(String oldTrainerUsername, String newTrainerUsername, String newTrainerCompleteName);

}
