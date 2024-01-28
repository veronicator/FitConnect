package it.unipi.dsmt.FitConnect.repositories;

import it.unipi.dsmt.FitConnect.entities.User;
import it.unipi.dsmt.FitConnect.enums.UserRole;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.data.mongodb.repository.Query;
import org.springframework.data.mongodb.repository.Update;

import java.util.List;
import java.util.Optional;

public interface UserRepository extends MongoRepository<User, String> {

    /** find methods */
    List<User> findByFirstname(String firstname);
    List<User> findByLastname(String lastname);
    Optional<User> findByUsername(String username);
    Optional<User> findByEmail(String email);
    List<User> findByRole(UserRole role);

    /** exists methods */
    boolean existsByFirstname(String firstname);
    boolean existsByLastname(String lastname);
    boolean existsByEmail(String email);
    boolean existsByUsername(String username);

    /** delete methods */
    void deleteByEmail(String email);
    void deleteByUsername(String username);

    /** update methods */
    @Query("{'email': ?0 }")
    @Update("{ $set: {'email': ?1 }}")
    void updateEmail(String oldEmail, String newEmail);

    @Query("{'password': ?0 }")
    @Update("{ $set: {'password': ?1 }}")
    void updatePassword(String oldPwd, String newPwd);

}
