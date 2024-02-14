package it.unipi.dsmt.FitConnect.entities;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class GeneralUser {
    protected String firstName;
    protected String lastName;
    protected String email;

    public GeneralUser(String firstName, String lastName, String email) {
        this.firstName = firstName;
        this.lastName = lastName;
        this.email = email.toLowerCase();
    }
}