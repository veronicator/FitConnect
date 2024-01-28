package it.unipi.dsmt.FitConnect.services;

import it.unipi.dsmt.FitConnect.entities.User;
import it.unipi.dsmt.FitConnect.enums.UserRole;
import it.unipi.dsmt.FitConnect.repositories.UserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Optional;

@Service
public class UserService {
    @Autowired
    private UserRepository userRepository;

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

    public Optional<User> findById(String id) {
        return userRepository.findById(id);
    }

    public Optional<User> findByEmail(String email){
        return userRepository.findByEmail(email);
    }

    public Optional<User> findByUsername(String username) {
        return userRepository.findByUsername(username);
    }
    public List<User> findAllUsers() {
        return userRepository.findAll();
    }

    public List<User> findAllByFirstName(String firstname) {
        return userRepository.findByFirstname(firstname);
    }

    public List<User> findAllByLastName(String lastname) {
        return userRepository.findByLastname(lastname);
    }

    public List<User> findByRole(UserRole role) {
        return userRepository.findByRole(role);
    }

    public boolean existsByFirstName(String firstname) {
        return userRepository.existsByFirstname(firstname);
    }

    public boolean existsByLastName(String lastname) {
        return userRepository.existsByLastname(lastname);
    }

    public boolean existsByEmail(String email) {
        return userRepository.existsByEmail(email);
    }

    public boolean existsByUsername(String username) {
        return userRepository.existsByUsername(username);
    }

    public void deleteById(String id) {
        userRepository.deleteById(id);
    }
    public void deleteByEmail(String email) {
        userRepository.deleteByEmail(email);
    }

    public void deleteUser(User user) {
        userRepository.delete(user);
    }

    public boolean updateEmail(String oldEmail, String newEmail) {
        if(existsByEmail(newEmail)) {
            System.out.println("Email address already used by another account");
            return false;
        }
        userRepository.updateEmail(oldEmail, newEmail);
        return true;
    }
}
