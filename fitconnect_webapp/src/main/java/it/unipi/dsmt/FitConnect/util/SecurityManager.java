package it.unipi.dsmt.FitConnect.util;

import lombok.NoArgsConstructor;
import org.springframework.stereotype.Service;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.util.Base64;
@Service
@NoArgsConstructor
public class SecurityManager {

    public String digestSHA(final String password) {
        String base64;
        try {
            MessageDigest digest = MessageDigest.getInstance("SHA256");
            digest.update(password.getBytes());
            base64 = Base64
                    .getEncoder()
                    .encodeToString(digest.digest());
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException(e);
        }
        return "{SHA}" + base64;
    }

//    public String get_SHA_256_SecurePassword(String passwordToHash,
//                                                     String salt) {
//        String generatedPassword = null;
//        try {
//            MessageDigest md = MessageDigest.getInstance("SHA-256");
//            md.update(salt.getBytes());
//            byte[] bytes = md.digest(passwordToHash.getBytes());
//            StringBuilder sb = new StringBuilder();
//            for (int i = 0; i < bytes.length; i++) {
//                sb.append(Integer.toString((bytes[i] & 0xff) + 0x100, 16)
//                        .substring(1));
//            }
//            generatedPassword = sb.toString();
//        } catch (NoSuchAlgorithmException e) {
//            e.printStackTrace();
//        }
//        return generatedPassword;
//    }
//
//    // Add salt
//    public String getSalt() throws NoSuchAlgorithmException {
//        SecureRandom sr = SecureRandom.getInstance("SHA1PRNG");
//        byte[] salt = new byte[16];
//        sr.nextBytes(salt);
//        return salt.toString();
//    }
//
//    public String getStaticSalt(){
//        return "my_static_salt";
//    }

}

