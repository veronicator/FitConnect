package it.unipi.dsmt.FitConnect.controller;


import it.unipi.dsmt.FitConnect.entities.LdapUser;
import it.unipi.dsmt.FitConnect.entities.MongoUser;
import it.unipi.dsmt.FitConnect.services.AuthService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;


@Controller
public class LoginController {
     @Autowired
     AuthService authService;

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

    @PostMapping("/login")
    public String doLogin(@RequestParam String username,
                          @RequestParam String password,
                          Model model) {

        MongoUser loggedUser = authService.authenticate(username, password);

        if(loggedUser == null)
            return "login";
        else{
            model.addAttribute("username", loggedUser.getUsername());
            return "home";
        }
    }

    @PostMapping("/signup")
    public String doSignup(@RequestParam String firstName, @RequestParam String lastName,
                           @RequestParam String email, @RequestParam String username,
                           @RequestParam String password, @RequestParam String repeat_password)
    {
        if(!password.equals(repeat_password)) {
            System.out.println("Password mismatch. Retry");
            return "signup";
        }

        try{
            String commonName = String.join(" ", firstName, lastName);
            LdapUser ldapUser = new LdapUser(username, commonName, lastName, password);
            MongoUser mongoUser = new MongoUser(username, firstName, lastName, email);
            //todo: gestire ruolo
            mongoUser.setRole("client");
            authService.signup(ldapUser, mongoUser);

            return "login";

        } catch (Exception e){
            System.out.println("Error in signup: " + e.getMessage());
            return "signup";
        }
    }

}
