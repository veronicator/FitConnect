package it.unipi.dsmt.FitConnect.services;

import it.unipi.dsmt.FitConnect.entities.User;
import it.unipi.dsmt.FitConnect.enums.UserRole;
import it.unipi.dsmt.FitConnect.repositories.UserRepository;
import it.unipi.dsmt.FitConnect.entities.MongoUser;
import it.unipi.dsmt.FitConnect.repositories.mongo.MongoUserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class UserService {

    @Autowired
    private MongoUserRepository mongoUserRepository;
    @Autowired UserRepository userRepository;

    public User createNewUser(String firstname, String lastname, String username,
                              String email, String pwd, UserRole role) {
        if (existsByEmail(email)) { // todo: || existsByUsername(username) aggiungere username
            System.out.println("email/username already used for another account");
            return null;
        }
        return userRepository.insert(new User(firstname, lastname, username, email, pwd, role));
    }

    // temporanea -> da eliminare
    public User createNewUser(String firstname, String lastname,
                              String email, String pwd, String role) {
        if (existsByEmail(email)) { // todo: || existsByUsername(username) aggiungere username
            System.out.println("email/username already used for another account");
            return null;
        }
        return userRepository.insert(new User(firstname, lastname, "username", email, pwd, UserRole.client));
    }

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
        return mongoUserRepository.findByFirstName(firstname);
    }

    public List<MongoUser> findAllByLastName(String lastname) {
        return mongoUserRepository.findByLastName(lastname);
    }

    public List<MongoUser> findByRole(String role) {
        return mongoUserRepository.findByRole(role);
    }

    public boolean existsByFirstName(String firstname) {
        return mongoUserRepository.existsByFirstName(firstname);
    }

    public boolean existsByLastName(String lastname) {
        return mongoUserRepository.existsByLastName(lastname);
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
