package it.unipi.dsmt.fitconnect.services;

import it.unipi.dsmt.fitconnect.entities.LdapUser;
import it.unipi.dsmt.fitconnect.entities.MongoUser;
import it.unipi.dsmt.fitconnect.repositories.ldap.LdapUserRepository;
import it.unipi.dsmt.fitconnect.util.SecurityManager;
import lombok.NoArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.ldap.core.DirContextAdapter;
import org.springframework.ldap.core.LdapTemplate;
import org.springframework.ldap.support.LdapNameBuilder;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import javax.naming.Name;

@NoArgsConstructor
@Service
public class AuthService {

    @Autowired
    private LdapTemplate ldapTemplate;

    @Autowired
    @Qualifier("ldapUserRepository")
    private LdapUserRepository ldapUserRepository;

    @Autowired
    private DBService dbService;

    @Autowired
    private SecurityManager securityManager;

    /**
     * Function for the login phase. User get authentication in ldap database. Then it returns user data from mongodb
     * @param username: username entered in login form from the user
     * @param password: password entered in login form from the user
     * @return MongoUser: user data stored in mongodb
     */
    public MongoUser authenticate(final String username, final String password) {
        try {
            // Check if credentials are valid
            if (!isValidCredentials(username, password)) {
//                logger.info("Invalid credentials");
                System.out.println("Invalid credentials");
                return null;
            }

            // Retrieve user from LDAP repository
            // findByUsernameAndPassword checks also the password
            LdapUser ldapUser = ldapUserRepository.findByUsernameAndPassword(username, securityManager.hashPassword(password));

            // Check if user exists
            if (ldapUser == null) {
//                logger.info("User not found");
                System.out.println("Authentication failed");
                return null;
            }

            // Retrieve user from MongoDB
            MongoUser loggedUser = dbService.getUser(username);

            // Return authenticated user
            if (loggedUser != null) {
//                logger.info("Authentication succeeded!");
//                logger.info("User logged: " + loggedUser.get());
                System.out.println("Authentication succeeded!");
                System.out.println("User logged: " + loggedUser);

                return loggedUser;
            } else {
                System.out.println("User not found in mongodb");
                return null;
            }
        } catch (Exception e) {
//            logger.error("Error in authentication phase: " + e.getMessage());
            System.out.println("Error in authentication phase: " + e.getMessage());
            return null;
        }
    }

    /**
     * Check if login input fields are valid
     * @param username: username entered in login form from the user
     * @param password: password entered in login form from the user
     * @return
     */
    private boolean isValidCredentials(String username, String password) {
        return StringUtils.hasText(username) && StringUtils.hasText(password);
    }

    /**
     * This function try to modify password of the user if he is into ldap database
     * @param username: user that want to change password
     * @param password: new password
     */
    public void modifyPassword(final String username, final String password) {
        try{
            LdapUser user = ldapUserRepository.findByUsername(username);
            user.setPassword(securityManager.hashPassword(password));
            ldapUserRepository.save(user);
            System.out.println("Password modified");
        } catch (Exception e) {
            System.err.println("Error modifying user: " + e.getMessage());
        }
    }

    /**
     * Function for retrieve a user in ldap database.
     * Used for check if username is already used in signup phase
     * @param username: username to find
     * @return LdapUser | null
     */
    public LdapUser search(final String username) {

        try {
            LdapUser user = ldapUserRepository.findByUsername(username);
            // Logging: Utilizzare il framework di logging appropriato
            System.out.println("Found user: " + user);
            return user;
        } catch (Exception e) {
            // Logging e gestione delle eccezioni
            System.err.println("Error searching user: " + e.getMessage());
            return null;
        }
    }

    /**
     * Function for signup phase. Try to insert user data in ldap and mongo databases
     * @param ldapNewUser: user data to insert in ldap
     * @param mongoNewUser: user data to insert in mongodb
     * @throws Exception
     */
    public void signup(LdapUser ldapNewUser, MongoUser mongoNewUser) throws Exception {
        System.out.println("in signup");

        // check if username is already used
        if(search(ldapNewUser.getUsername()) != null
                || dbService.existsByUsername(mongoNewUser.getUsername())) {
            throw new Exception("Username already used. Retry");
        } else
            System.out.println("username ok");

        if (createLdapUser(ldapNewUser)) {
            if (dbService.createMongoUser(mongoNewUser)) {
                return;
            }
            ldapUserRepository.delete(ldapNewUser);
        }
        throw new Exception("Error in registration phase");

    }

    /**
     * Function for creating user object to store into ldap database
     * @param user: user data
     * @return true|false
     */
    public boolean createLdapUser(final LdapUser user) {
        System.out.println("create...");

        Name dn = LdapNameBuilder
                .newInstance()
                .add("dc", "com").add("dc", "fitconnect").add("ou", "fit").add("uid", user.getUsername())
                .build();

        DirContextAdapter context = new DirContextAdapter(dn);
        context.setAttributeValues("objectclass", new String[]{"top", "organizationalPerson", "inetOrgPerson"});
        context.setAttributeValue("cn", user.getCommonName());
        context.setAttributeValue("sn", user.getLastName());
        String hashedPassword = securityManager.hashPassword(user.getPassword());
        context.setAttributeValue("userPassword", hashedPassword);

        try {
            // insert user into ldap db
            ldapTemplate.bind(context);
            System.out.println("User: " + user.getUsername() + " registered in ldap");
            return true;
        } catch(Exception e) {
            System.out.println("error in ldap registration: " + e.getMessage());
            return false;
        }
    }

}
