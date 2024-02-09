package it.unipi.dsmt.fitconnect.repositories.mongo;

import it.unipi.dsmt.fitconnect.entities.Course;
import it.unipi.dsmt.fitconnect.entities.MongoUser;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.data.mongodb.repository.*;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface CourseRepository extends MongoRepository<Course, String> {

    /** findBy methods */
    //DEFAULT (case-insensitive) {"firstname" : { $regex: firstname, $options: 'i'}}
    Page<Course> findAll(Pageable pageable);
    @Query("{'courseName': { $regex: ?0, $options: 'i'}}")
    List<Course> findByCourseName(String courseName);
    List<Course> findByTrainer(String trainer);
    @Query("{'schedules.dayOfTheWeek': ?0}")
    List<Course> findByDay(String day);
    @Query("{'courseName': { $regex: ?0, $options: 'i'}, 'trainerUsername': { $regex: ?1, $options: 'i'} }")
    Optional<Course> findByCourseNameAndTrainer(String course, String trainerUsername);
    @Query("{'id': ?0, 'enrolled.username': ?1 }")
    List<Course> findByIdAndUser(String id, String username);
    @Query("{'enrolled.username': ?0}")
    List<Course> findByUser(String username);

    /** existsBy methods */
    boolean existsByCourseName(String courseName);
    boolean existsByTrainer(String trainer);
    @Query(value = "{'courseName': { $regex: ?0, $options: 'i'}, 'trainerUsername': { $regex: ?1, $options: 'i'} }", exists = true)
    boolean existsByCourseNameAndTrainer(String courseName, String trainerUsername);

    /** deleteBy methods */
    void deleteByTrainer(String trainer);

    @Query("{'id': ?0, 'enrolled.username': ?1 }")
    @Update("{ $pull: { 'enrolled': { 'username': ?1 }}}")
    void removeSubscription(String courseId, String username);

//    @Query
//    void removeSchelude(String courseId, Schedule schedule);

    /** update methods */
    @Query("{'_id': ?0 }")
    @Update("{ $push: { 'enrolled': ?1 }}")
    void updateEnrolledList(String courseId, MongoUser user);

    @Query("{'id': ?0 }")
    @Update("{ $set: {'courseName': ?1 }}")
    void updateCourseName(String courseId, String newName);

    @Query("{'courseName': ?0 }")
    @Update("{ $set: {'courseName': ?1 }}")
    void updateMultiCourseName(String oldName, String newName);

    @Query("{'trainer': ?0 }")
    @Update("{ $set: {'trainer': ?1 }}")
    void updateTrainer(String oldTrainer, String newTrainer);

}
