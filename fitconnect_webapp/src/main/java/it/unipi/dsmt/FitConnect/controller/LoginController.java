package it.unipi.dsmt.FitConnect.controller;

import it.unipi.dsmt.FitConnect.entities.User;
import it.unipi.dsmt.FitConnect.services.AuthService;
import it.unipi.dsmt.FitConnect.services.UserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

@Controller
public class LoginController {
     @Autowired
     UserService userService;
//     @Autowired
//     AuthService authService;

//     @GetMapping("/debug")
//     public String debug(Model model) throws NoSuchAlgorithmException {
//         List<User> users = userService.findAllUsers();
//         List<String> hashedpsws = new ArrayList<>();
//         for(User user : users){
//             String s = user.getPassword();
//             hashedpsws.add(authService.hashPassword(s));
//         }
//
//         model.addAttribute("users", users);
//         model.addAttribute("passwords", hashedpsws);
//         return "debug";
//     }

    @GetMapping("/home")
    public String login() {
        return "home";
    }

    //@GetMapping({"/", "/login"})
    @GetMapping(value = {"/","/login"})
    public String index(){
        return "login";
    }

    @GetMapping("/signup")
    public String signup(){ return "signup"; }


    /*
    * Login handler
    */
    @PostMapping("/login")
    public String doLogin(@RequestParam String username,
                          @RequestParam String password,
                          Model model) {

        User loggedUser = userService.authenticate(username, password);

        if(loggedUser == null)
            return "login";
        else{
            model.addAttribute("username", loggedUser.getUsername());
            return "home";
        }
    }

    /*
     * Signup handler
     */
    @PostMapping("/signup")
    public String doSignup(@RequestParam String name, @RequestParam String surname,
                           @RequestParam String email, @RequestParam String username,
                           @RequestParam String password, @RequestParam String repeat_password,
                           Model model) {

        if(!password.equals(repeat_password)) {
            System.out.println("Password mismatch. Retry");
            return "signup";
        }

//        if(userService.search(username) != null){
//            System.out.println("Username already used. Retry");
//            return "signup";
//        }

        try{
            User newUser = new User(username, name, surname, password, email);

            userService.signup(newUser);

            return "login";

        } catch (Exception e){
            System.out.println("Signup error: " + e.getMessage());
            return "signup";
        }
    }

}
