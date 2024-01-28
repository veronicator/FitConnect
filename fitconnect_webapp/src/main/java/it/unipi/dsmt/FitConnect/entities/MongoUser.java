package it.unipi.dsmt.FitConnect.entities;

import lombok.*;
import org.springframework.data.mongodb.core.mapping.*;

@Getter
@Setter
@Document(collection = "users")
public class MongoUser {

    @MongoId
    private String mongoId;
    private String username;    //unique for the user
    private String firstName;
    private String lastName;
    private String email;
    private String role;    // client | PT | admin

    public MongoUser(){}

    public MongoUser(String username, String firstName, String lastName, String email, String role) {
        this.username = username;
        this.firstName = firstName;
        this.lastName = lastName;
        this.email = email;
        this.role = role;
    }

    public MongoUser(String username, String firstName, String lastName, String email) {
        this.username = username;
        this.firstName = firstName;
        this.lastName = lastName;
        this.email = email;
    }

    @Override
    public String toString() {
        return "MongoUser{" +
                "username='" + username + '\'' +
                ", firstName='" + firstName + '\'' +
                ", lastName='" + lastName + '\'' +
                ", email='" + email + '\'' +
                ", role='" + role + '\'' +
                '}';
    }
}

