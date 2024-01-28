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
    List<MongoUser> findByFirstName(String firstName);
    List<MongoUser> findByLastName(String lastName);
    Optional<MongoUser> findByUsername(String username);
    Optional<MongoUser> findByEmail(String email);
    List<MongoUser> findByRole(String role);

    /** exists methods */
    boolean existsByFirstName(String firstName);
    boolean existsByLastName(String lastName);
    boolean existsByEmail(String email);
    boolean existsByUsername(String username);

    /** delete methods */
    void deleteByEmail(String email);

    /** update methods */
    @Query("{'email': ?0 }")
    @Update("{ $set: {'email': ?1 }}")
    void updateEmail(String oldEmail, String newEmail);

}
