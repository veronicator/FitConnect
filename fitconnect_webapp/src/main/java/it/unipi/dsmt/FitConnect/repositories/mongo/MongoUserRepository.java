package it.unipi.dsmt.FitConnect.repositories.mongo;

import it.unipi.dsmt.FitConnect.entities.Course;
import it.unipi.dsmt.FitConnect.entities.MongoUser;
import it.unipi.dsmt.FitConnect.enums.UserRole;
import org.bson.types.ObjectId;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.data.mongodb.repository.*;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
@Repository("mongoUserRepository")
public interface MongoUserRepository extends MongoRepository<MongoUser, String> {

    /** find methods */
    @Query("{'firstname' : { $regex: ?0, $options: 'i'} }")
    List<MongoUser> findByFirstname(String firstname);
    @Query("{'lastname' : { $regex: ?0, $options: 'i'} }")
    List<MongoUser> findByLastname(String lastname);
    @Query("{'username' : { $regex: ?0, $options: 'i'} }")
    Optional<MongoUser> findByUsername(String username);
    @Query("{'email' : { $regex: ?0, $options: 'i'} }")
    Optional<MongoUser> findByEmail(String email);
    List<MongoUser> findByRole(UserRole role);

    // da testare se va passato l'id o l'oggetto Course
    List<MongoUser> findByCourse(String courseId);

    /** exists methods */
    @Query(value = "{'firstname' : { $regex: ?0, $options: 'i'} }", exists = true)
    boolean existsByFirstname(String firstname);
    @Query(value = "{'lastname' : { $regex: ?0, $options: 'i'} }", exists = true)
    boolean existsByLastname(String lastname);
    @Query(value = "{'email' : { $regex: ?0, $options: 'i'} }", exists = true)
    boolean existsByEmail(String email);
    @Query(value = "{'username' : { $regex: ?0, $options: 'i'} }", exists = true)
    boolean existsByUsername(String username);

    /** delete methods */
    @Query(value = "{'email' : { $regex: ?0, $options: 'i'} }", delete = true)
    void deleteByEmail(String email);
    @Query(value = "{'username' : { $regex: ?0, $options: 'i'} }", delete = true)
    void deleteByUsername(String username);

    /** update methods */
    @Query("{ 'courses': ?0 }")
    @Update("{ $pull: { 'courses': ?0 } }")
    void removeCourse(ObjectId course);
    @Query("{'email': { $regex: ?0, $options: 'i'} }")
    @Update("{ $set: {'email': ?1 } }")
    void updateEmail(String oldEmail, String newEmail);

    @Query("{'username' : { $regex: ?0, $options: 'i'} }")
    @Update("{ $push: {'courses': ?1 } }")
    void updateCourseList(String username, Course course);
//    void updateCourseList(String username, String courseId);

}
