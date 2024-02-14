package it.unipi.dsmt.fitconnect.entities;

import it.unipi.dsmt.fitconnect.enums.UserRole;
import lombok.*;
import org.bson.types.ObjectId;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.ReadOnlyProperty;
import org.springframework.data.mongodb.core.index.Indexed;
import org.springframework.data.mongodb.core.mapping.*;

import java.util.ArrayList;
import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Document(collection = "users")
public class MongoUser {

    @Id
    private ObjectId id;
    @Indexed(unique = true)
    private String username;    //unique for the user
    private String firstname;
    private String lastname;
    private String email;
    private UserRole role;    // client | trainer | invalid

    @DocumentReference(collection = "courses", lazy = true)
    List<Course> courses;   // id corsi a cui è iscritto l'utente "client", o corsi insegnati se "trainer"

    @ReadOnlyProperty
    @DocumentReference(collection = "reservations", lookup = "{ 'bookedUsers': ?#{#self.username} }", lazy = true)
    List<Reservations> reservations;    // active reservations of the user (not stored in the db, automatic reference)

    public MongoUser(String username, String firstname, String lastname, String email, UserRole role) {
        this.username = username;
        this.firstname = firstname;
        this.lastname = lastname;
        this.email = email.toLowerCase();
        this.role = role;
    }

    public MongoUser(String username, String firstname, String lastname, String email) {
        this(username, firstname, lastname, email, UserRole.client);
    }

    public String getCompleteName() {
        return String.format("%s %s", firstname, lastname);
    }

    public boolean addCourse(Course c) {
        if (courses == null)
            courses = new ArrayList<>();
        if (courses.contains(c))
            return false;
        return courses.add(c);
    }

    public boolean removeCourse(Course c) {
        if (courses == null)
            return false;
        return courses.remove(c);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this)
            return true;
        if (!(obj instanceof MongoUser user))
            return false;
        return user.username.equals(this.username);
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

