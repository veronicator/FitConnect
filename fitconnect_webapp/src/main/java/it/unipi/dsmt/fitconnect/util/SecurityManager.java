package it.unipi.dsmt.fitconnect.util;

import lombok.NoArgsConstructor;
import org.springframework.stereotype.Service;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Base64;

@Service
@NoArgsConstructor
public class SecurityManager {

    /**
     * Hashes a password using the SHA algorithm and encodes it in Base64 with the "{SHA}" prefix.
     *
     * @param password the password to hash
     * @return the hashed password with the "{SHA}" prefix
     */
    public String hashPassword(final String password){
        String base64;
        try {
            MessageDigest digest = MessageDigest.getInstance("SHA");
            digest.update(password.getBytes());
            base64 = Base64
                    .getEncoder()
                    .encodeToString(digest.digest());
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException(e);
        }

        return "{SHA}" + base64;
    }


}

