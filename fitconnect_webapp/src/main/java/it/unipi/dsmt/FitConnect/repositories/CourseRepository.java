package it.unipi.dsmt.FitConnect.repositories;

import it.unipi.dsmt.FitConnect.entities.Course;
import it.unipi.dsmt.FitConnect.entities.GeneralUser;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.data.mongodb.repository.Query;
import org.springframework.data.mongodb.repository.Update;

import java.util.List;

public interface CourseRepository extends MongoRepository<Course, String> {

    /** findBy methods */
    List<Course> findByCourseName(String courseName);
    List<Course> findByTrainer(String trainer);
    @Query("{'schedules.dayOfTheWeek': ?0}")
    List<Course> findByDay(String day);

    @Query("{'id': ?0, 'enrolled.email': ?1 }")
    List<Course> findByIdAndEmail(String id, String email);

    /** existsBy methods */
    boolean existsByCourseName(String courseName);
    boolean existsByTrainer(String trainer);

    /** deleteBy methods */
    void deleteByTrainer(String trainer);

    @Query("{'id': ?0, 'enrolled.email': ?1 }")
    @Update("{ $pull: { 'enrolled': { 'email': ?1 }}}")
    void removeSubscription(String courseId, String userEmail);

    /** update methods */
    @Query("{'_id': ?0 }")
    @Update("{ $push: { 'enrolled': ?1 }}")
    void updateEnrolledList(String courseId, GeneralUser user);

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
