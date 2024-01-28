package it.unipi.dsmt.FitConnect.entities;

import it.unipi.dsmt.FitConnect.enums.UserRole;
import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.ReadOnlyProperty;
import org.springframework.data.mongodb.core.mapping.*;

import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Document(collection = "users")
public class MongoUser {

    @Id
    private String id;
    private String username;    //unique for the user
    private String firstname;
    private String lastname;
    private String email;
    private UserRole role;    // client | PT | admin

    //    @ReadOnlyProperty   // persistente o non persistente?
    @DocumentReference(collection = "courses", lazy = true)
    List<Course> courses;   // id corsi a cui è iscritto l'utente, se "client", o corsi insegnati se "trainer"
                            // null se "admin"
    @ReadOnlyProperty
    @DocumentReference(collection = "schedules", lazy = true)
    List<Schedule> reservations;    // id schedule classi prenotate, solo se "client", otherwise null

    public MongoUser(String username, String firstName, String lastName, String email, UserRole role) {
        this.username = username;
        this.firstname = firstName;
        this.lastname = lastName;
        this.email = email;
        this.role = role;
    }

    public MongoUser(String username, String firstName, String lastName, String email) {
        this.username = username;
        this.firstname = firstName;
        this.lastname = lastName;
        this.email = email;
    }

    @Override
    public String toString() {
        return "MongoUser{" +
                "username='" + username + '\'' +
                ", firstName='" + firstname + '\'' +
                ", lastName='" + lastname + '\'' +
                ", email='" + email + '\'' +
                ", role='" + role + '\'' +
                '}';
    }
}

