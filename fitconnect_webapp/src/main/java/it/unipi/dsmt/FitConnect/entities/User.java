package it.unipi.dsmt.FitConnect.entities;

import lombok.Getter;
import lombok.Setter;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

@Getter
@Setter
@Document(collection = "users")
public class User extends GeneralUser {

    @Id
    private String id;
//    private String username;    // todo: modificare login form per far inserire anche l'username
    private String password;
    private String role;    // client | PT | admin

    public User(String firstName, String lastName, String email, String password, String role) {
        super(firstName, lastName, email, firstName);   // todo: togliere costruttore
        this.password = password;
        this.role = role;
    }

    public User(String firstName, String lastName, String username, String email, String password, String role) {
        super(firstName, lastName, email, username);
        this.password = password;
        this.role = role;
    }

    @Override
    public String toString() {
        return String.format(
                "User[name='%s', surname='%s', email='%s', role='%s']",
                firstName, lastName, email, role
        );
    }
}

