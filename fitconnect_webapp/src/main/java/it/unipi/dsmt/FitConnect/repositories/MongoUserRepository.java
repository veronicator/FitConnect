package it.unipi.dsmt.FitConnect.repositories;

import it.unipi.dsmt.FitConnect.entities.User;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.data.mongodb.repository.Query;
import org.springframework.data.mongodb.repository.Update;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;
@Repository
public interface MongoUserRepository extends MongoRepository<User, String> {

    /** find methods */
    List<User> findByFirstName(String firstName);
    List<User> findByLastName(String lastName);
    User findByUsername(String username);
    Optional<User> findByEmail(String email);
    List<User> findByRole(String role);

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
