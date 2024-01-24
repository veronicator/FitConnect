package it.unipi.dsmt.FitConnect.entities;

import lombok.Getter;
import lombok.Setter;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.ldap.odm.annotations.Attribute;
import org.springframework.ldap.odm.annotations.Entry;

import javax.naming.Name;

@Getter
@Setter
@Document(collection = "users")
@Entry(base = "ou=people,dc=baeldung,dc=com", objectClasses = {"top", "person", "organizationalPerson", "inetOrgPerson"})
public class User {

    @Id
    private Name id;            // todo: controllare con mongo che succede
    @Attribute(name = "uid")
    private String username;
    @Attribute(name = "cn")
    private String firstName;
    @Attribute(name = "sn")
    private String lastName;
    @Attribute(name = "userPassword")
    private String password;
    private String email;
    private String role;    // client | PT | admin

    public User(String username, String firstName, String lastName, String password, String email) {    //Constructor without role
        this.username = username;
        this.firstName = firstName;
        this.lastName = lastName;
        this.password = password;
        this.email = email;
    }

    public User(String username, String firstName, String lastName, String password, String email, String role) {   //Constructor with role
        this.username = username;
        this.firstName = firstName;
        this.lastName = lastName;
        this.password = password;
        this.email = email;
        this.role = role;
    }

    @Override
    public String toString() {
        return "User{" +
                "id=" + id +
                ", username='" + username + '\'' +
                ", firstName='" + firstName + '\'' +
                ", lastName='" + lastName + '\'' +
                ", password='" + password + '\'' +
                ", email='" + email + '\'' +
                ", role='" + role + '\'' +
                '}';
    }
}

