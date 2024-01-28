package it.unipi.dsmt.FitConnect.repositories.mongo;

import it.unipi.dsmt.FitConnect.entities.MongoUser;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.data.mongodb.repository.*;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
@Repository("mongoUserRepository")
public interface MongoUserRepository extends MongoRepository<MongoUser, String> {

    /** find methods */
    List<MongoUser> findByFirstname(String firstName);
    List<MongoUser> findByLastname(String lastName);
    Optional<MongoUser> findByUsername(String username);
    Optional<MongoUser> findByEmail(String email);
    List<MongoUser> findByRole(String role);

    /** exists methods */
    boolean existsByFirstname(String firstName);
    boolean existsByLastname(String lastName);
    boolean existsByEmail(String email);
    boolean existsByUsername(String username);

    /** delete methods */
    void deleteByEmail(String email);
    void deleteByUsername(String username);

    /** update methods */
    @Query("{'email': ?0 }")
    @Update("{ $set: {'email': ?1 }}")
    void updateEmail(String oldEmail, String newEmail);

}
