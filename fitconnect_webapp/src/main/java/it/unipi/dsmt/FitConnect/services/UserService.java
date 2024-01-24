package it.unipi.dsmt.FitConnect.services;

import it.unipi.dsmt.FitConnect.entities.User;
import it.unipi.dsmt.FitConnect.repositories.LdapUserRepository;
import it.unipi.dsmt.FitConnect.repositories.MongoUserRepository;
import it.unipi.dsmt.FitConnect.util.SecurityManager;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.env.Environment;
import org.springframework.ldap.core.ContextSource;
import org.springframework.ldap.core.DirContextAdapter;
import org.springframework.ldap.core.LdapTemplate;
import org.springframework.ldap.support.LdapNameBuilder;
import org.springframework.stereotype.Service;

import javax.naming.Name;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Base64;

@Service
public class UserService {

    @Autowired
    private Environment env;

    @Autowired
    private ContextSource contextSource;

    @Autowired
    private LdapTemplate ldapTemplate;

    @Autowired
    private LdapUserRepository ldapUserRepository;

    @Autowired
    private MongoUserRepository mongoUserRepository;

    @Autowired
    private SecurityManager securityManager;

    public User authenticate(final String username, final String password) {
        User user = ldapUserRepository.findByUsernameAndPassword(username, securityManager.digestSHA(password));
        if(user == null) {
            System.out.println("Authentication failed");
            System.out.println("Failed in ldap database");
            return null;
        }
        else {
            user = mongoUserRepository.findByUsername(username);
            if(user == null){
                System.out.println("Authentication failed");
                System.out.println("Failed in mongo database");
                return null;
            }
            else{
                System.out.println("Authenticaiton succeded!");
                return user;
            }
        }
    }

    public void create(final User user) {
        System.out.println("create...");

        Name dn = LdapNameBuilder
                .newInstance()
                .add("dc", "com").add("dc", "baeldung").add("ou", "people").add("uid", user.getUsername())
                .build();

        DirContextAdapter context = new DirContextAdapter(dn);
        context.setAttributeValues("objectclass", new String[]{"top", "person", "organizationalPerson", "inetOrgPerson"});
        context.setAttributeValue("cn", user.getFirstName());
        context.setAttributeValue("sn", user.getLastName());
        context.setAttributeValue("userPassword", securityManager.digestSHA(user.getPassword()));

        System.out.println(context);

//        try {
//            // insert user into ldap db
//            ldapTemplate.bind(context);
//            // insert user into mongo db
//            mongoUserRepository.save(user);
//            System.out.println("User: " + user.getUsername() + " registered");
//        } catch (Exception e) {
//            System.err.println("Error creating user: " + e.getMessage());
//        }

        // insert user into ldap db
        ldapTemplate.bind(context);
        // insert user into mongo db
        mongoUserRepository.save(user);
        System.out.println("User: " + user.getUsername() + " registered");
    }

    public User search(final String username) {

        User user = ldapUserRepository.findByUsername(username);
        System.out.println("Found user: " + user);

        return user;
    }

    public void modify(final String username, final String password) {
        try{
            User user = ldapUserRepository.findByUsername(username);
            user.setPassword(securityManager.digestSHA(password));
            ldapUserRepository.save(user);
            System.out.println("Password modified");
        } catch (Exception e) {
            System.err.println("Error modifying user: " + e.getMessage());
        }

    }

    public void signup(final User newUser) throws Exception {

        // check if username is already used
        if(search(newUser.getUsername()) != null){
            throw new Exception("Username already used. Retry");
        }

//        try{
//            //add new user in ldap db
//            create(newUser);
//
//            //add new user in mongodb
//            mongoUserRepository.save(newUser);
//        }
//        catch (Exception e){
//            throw new Exception(e.getMessage());
//        }

        //add new user in ldap db
        create(newUser);

        //add new user in mongodb
        mongoUserRepository.save(newUser);

    }




}
