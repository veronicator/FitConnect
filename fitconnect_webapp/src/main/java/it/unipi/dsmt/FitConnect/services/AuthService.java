package it.unipi.dsmt.FitConnect.services;

import it.unipi.dsmt.FitConnect.util.SecurityManager;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.security.NoSuchAlgorithmException;

@Service
public class AuthService {

//    @Autowired
//    private UserService userService;
//    private SecurityManager securityManager = new SecurityManager();
//
//    public boolean checkSignup(String email, String password){
//        boolean result = true;
//
//        //check email
//        if (userService.existsByEmail(email)) {
//            System.out.println("Email already in use");
//            result = false;
//        }
//
//        //TODO Check role (how to signup trainers)
//        String role = "user";
//
//        return result;
//    }
//
//    //TODO fare hash password
//    public String hashPassword(String password) throws NoSuchAlgorithmException {
//
//        String salt = securityManager.getStaticSalt();
//        String hashed = securityManager.get_SHA_256_SecurePassword(password, salt);
//
//        return hashed;
//    }




}
