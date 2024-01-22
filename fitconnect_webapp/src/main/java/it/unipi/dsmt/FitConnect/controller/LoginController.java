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
     @Autowired
     AuthService authService;

     @GetMapping("/debug")
     public String debug(Model model) throws NoSuchAlgorithmException {
         List<User> users = userService.findAllUsers();
         List<String> hashedpsws = new ArrayList<>();
         for(User user : users){
             String s = user.getPassword();
             hashedpsws.add(authService.hashPassword(s));
         }

         model.addAttribute("users", users);
         model.addAttribute("passwords", hashedpsws);
         return "debug";
     }

    @GetMapping("/home")
    public String login() {
        return "home";
    }

    //@GetMapping({"/", "/login"})
    @GetMapping("/login")
    public String index(){
        return "login";
    }

    @GetMapping("/signup")
    public String signup(){ return "signup"; }

    @GetMapping("/")
    public String indexx(){
        return "index";
    }

    /*
    * Login handler
    */
    @PostMapping("/login")
    public String doLogin(@RequestParam String email,
                          @RequestParam String password,
                          RedirectAttributes redirectAttributes,
                          Model model) throws NoSuchAlgorithmException
    {
        Optional<User> opUser = userService.findByEmail(email);

        if (opUser.isPresent()) {

            User user = opUser.get();
            //input password
            String hashedLoginPassword = authService.hashPassword(password);

            if(hashedLoginPassword.equals(user.getPassword())){
                System.out.println("login succeeded");
                model.addAttribute("user", user);
                return "home";
            }
            else{
                System.out.println("login failed: password incorrect");
                return "redirect:/login";
            }
        }
        else{
            System.out.println("login failed: user not present in db");
            return "login";
        }
    }

    /*
     * Signup handler
     */
    @PostMapping("/signup")
    public String doSignup(@RequestParam String name,
                           @RequestParam String surname,
                           @RequestParam String email,
                           @RequestParam String password,
                           Model model) throws NoSuchAlgorithmException
    {
        //TODO password with confirm-password maybe in javascript

        if(authService.checkSignup(email, password)) {
            //TODO da modificare come si gestisce il ruolo
            String role = "user";

            String hashedPsd = authService.hashPassword(password);

            User newUser = userService.createNewUser(name, surname, email, hashedPsd, role);
            if (newUser != null) {
                System.out.println("user added correctly");
                return "redirect:/login";
            }

        }
//        else{
            System.out.println("signup failed");
            return "signup";
//        }

    }
}
