package it.unipi.dsmt.fitconnect.services;

import it.unipi.dsmt.fitconnect.enums.UserRole;
import it.unipi.dsmt.fitconnect.entities.MongoUser;
import it.unipi.dsmt.fitconnect.repositories.mongo.MongoUserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class UserService {

    @Autowired
    private MongoUserRepository mongoUserRepository;

    public Optional<MongoUser> findById(String id) {
        return mongoUserRepository.findById(id);
    }

    public Optional<MongoUser> findByEmail(String email){
        return mongoUserRepository.findByEmail(email);
    }

    public Optional<MongoUser> findByUsername(String username) {
        return mongoUserRepository.findByUsername(username);
    }
    public List<MongoUser> findAllUsers() {
        return mongoUserRepository.findAll();
    }

    public List<MongoUser> findAllByFirstName(String firstname) {
        return mongoUserRepository.findByFirstname(firstname);
    }

    public List<MongoUser> findAllByLastName(String lastname) {
        return mongoUserRepository.findByLastname(lastname);
    }

    public List<MongoUser> findByRole(UserRole role) {
        return mongoUserRepository.findByRole(role);
    }

    public boolean existsByFirstName(String firstname) {
        return mongoUserRepository.existsByFirstname(firstname);
    }

    public boolean existsByLastName(String lastname) {
        return mongoUserRepository.existsByLastname(lastname);
    }

    public boolean existsByEmail(String email) {
        return mongoUserRepository.existsByEmail(email);
    }

    public boolean existsByUsername(String username) {
        return mongoUserRepository.existsByUsername(username);
    }

    public void deleteById(String id) {
        mongoUserRepository.deleteById(id);
    }
    public void deleteByEmail(String email) {
        mongoUserRepository.deleteByEmail(email);
    }

    public void deleteUser(MongoUser user) {
        mongoUserRepository.delete(user);
    }

    public boolean updateEmail(String oldEmail, String newEmail) {
        if(existsByEmail(newEmail)) {
            System.out.println("Email address already used by another account");
            return false;
        }
        mongoUserRepository.updateEmail(oldEmail, newEmail);
        return true;
    }
















}
