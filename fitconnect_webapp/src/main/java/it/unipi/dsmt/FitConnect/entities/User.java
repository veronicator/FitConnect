package it.unipi.dsmt.FitConnect.entities;

import it.unipi.dsmt.FitConnect.enums.UserRole;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.ReadOnlyProperty;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.DocumentReference;

import java.util.List;

@Getter
@Setter
@Document(collection = "users")
@AllArgsConstructor
public class User {

    @Id
    private String id;
    private String firstname;
    private String lastname;
    private String username;
    private String email;
    private String password;
    private UserRole role;    // client | trainer | admin
    //    @ReadOnlyProperty   // persistente o non persistente?
    @DocumentReference(collection = "courses", lazy = true)
    List<Course> courses;   // id corsi a cui è iscritto l'utente, se "client", o corsi insegnati se "trainer"
                            // null se "admin"
    @ReadOnlyProperty
    @DocumentReference(collection = "schedules", lazy = true)
    List<Schedule> reservations;    // id schedule classi prenotate, solo se "client", otherwise null
        // lasciare o togliere?

    public User(String firstname, String lastname, String username, String email, String password, UserRole role) {
        this.firstname = firstname;
        this.lastname = lastname;
        this.username = username.toLowerCase();
        this.email = email.toLowerCase();
        this.password = password;
        this.role = role;
    }

    @Override
    public String toString() {
        return String.format(
                "User [%s, complete name: %s %s, email: %s, role: %s]",
                username, firstname, lastname, email, role
        );
    }
}

