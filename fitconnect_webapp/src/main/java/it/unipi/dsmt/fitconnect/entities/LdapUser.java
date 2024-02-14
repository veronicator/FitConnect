package it.unipi.dsmt.fitconnect.entities;

import lombok.*;
import org.springframework.ldap.odm.annotations.Attribute;
import org.springframework.ldap.odm.annotations.Entry;
import org.springframework.ldap.odm.annotations.Id;

import javax.naming.Name;

@Getter
@Setter
@Entry(base = "ou=fit,dc=fitconnect,dc=com", objectClasses = {"top", "organizationalPerson", "inetOrgPerson"})
public final class LdapUser {

    @Id
    //@Attribute(name = "dn")
    private Name ldapId;

    @Attribute(name = "uid")
    private String username;

    @Attribute(name = "cn")
    private String commonName;  // first name + last name

    @Attribute(name = "sn")
    private String lastName;

    @Attribute(name = "userPassword")
    private String password;

    public LdapUser(){}

    public LdapUser(String username, String commonName, String lastName, String password) {
        this.username = username.toLowerCase();
        this.commonName = commonName;
        this.lastName = lastName;
        this.password = password;
    }

    @Override
    public String toString() {
        return "LdapUser{" +
                "ldapId=" + ldapId +
                ", username='" + username + '\'' +
                ", commonName='" + commonName + '\'' +
                ", lastName='" + lastName + '\'' +
                ", password='" + password + '\'' +
                '}';
    }
}