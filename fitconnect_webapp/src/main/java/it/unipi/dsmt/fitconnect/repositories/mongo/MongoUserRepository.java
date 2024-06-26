package it.unipi.dsmt.fitconnect.repositories.mongo;

import it.unipi.dsmt.fitconnect.entities.MongoUser;
import it.unipi.dsmt.fitconnect.enums.UserRole;
import org.bson.types.ObjectId;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
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
    Page<MongoUser> findByRole(UserRole role, Pageable pageable);

    List<MongoUser> findByCourses(ObjectId courseId);

    @Query("{'role': ?0, 'courses': ?1 }")
    List<MongoUser> findByCourse(UserRole role, ObjectId course);

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

}
