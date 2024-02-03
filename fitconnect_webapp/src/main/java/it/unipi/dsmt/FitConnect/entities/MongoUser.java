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

    //    @ReadOnlyProperty
    @DocumentReference(collection = "courses", lazy = true)
    List<Course> courses;   // id corsi a cui è iscritto l'utente, se "client", o corsi insegnati se "trainer"

    @ReadOnlyProperty
    @DocumentReference(collection = "reservations", lookup = "{ 'bookedUsers': ?#{#self.username} }", lazy = true)
    List<Reservations> reservations;    // id reservationsDoc classi prenotate, solo se "client", otherwise null

    public MongoUser(String username, String firstName, String lastName, String email, UserRole role) {
        this.username = username.toLowerCase();
        this.firstname = firstName;
        this.lastname = lastName;
        this.email = email.toLowerCase();
        this.role = role;
    }

    public MongoUser(String username, String firstname, String lastname, String email) {
        this(username, firstname, lastname, email, UserRole.client);
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

