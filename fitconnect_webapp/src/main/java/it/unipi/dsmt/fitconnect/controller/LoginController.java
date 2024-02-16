package it.unipi.dsmt.fitconnect.controller;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;
import it.unipi.dsmt.fitconnect.entities.LdapUser;
import it.unipi.dsmt.fitconnect.entities.MongoUser;
import it.unipi.dsmt.fitconnect.enums.UserRole;
import it.unipi.dsmt.fitconnect.services.AuthService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;


@Controller
public class LoginController {
    @Autowired
    private AuthService authService;


    @GetMapping("/login")
    public String index() {
        return "login";
    }

    @GetMapping("/signup")
    public String signup() {
        return "signup";
    }

    @PostMapping("/login")
    public String doLogin(@RequestParam String username,
                          @RequestParam String password,
                          HttpServletRequest request,
                          Model model) {

        MongoUser loggedUser = authService.authenticate(username, password);

        if (loggedUser == null)
            return "login";
        else {
            HttpSession session = request.getSession(true);
            //session.setAttribute("loggedUser", loggedUser);
            session.setAttribute("uid", loggedUser.getId());
            session.setAttribute("username", loggedUser.getUsername());
            session.setAttribute("role", loggedUser.getRole());

            model.addAttribute("username", loggedUser.getUsername());

            return "home";
        }
    }

    @PostMapping("/signup")
    public String doSignup(@RequestParam String firstname, @RequestParam String lastname,
                           @RequestParam String email, @RequestParam String username,
                           @RequestParam String password, @RequestParam String repeat_password) {
        if (!password.equals(repeat_password)) {
            System.out.println("Password mismatch. Retry");
            return "signup";
        }

        try {
            String commonName = String.join(" ", firstname, lastname);
            LdapUser ldapUser = new LdapUser(username, commonName, lastname, password);
            MongoUser mongoUser = new MongoUser(username, firstname, lastname, email, UserRole.client);
            authService.signup(ldapUser, mongoUser);

            return "login";

        } catch (Exception e) {
            System.out.println("Error in signup: " + e.getMessage());
            return "signup";
        }
    }

//    @GetMapping("/login-error")
//    public String loginError(Model model) {
//        model.addAttribute("loginError", "Login Error");
//        return "login";
//    }

    @GetMapping("/logout")
    public String logout(HttpServletRequest request) {
        HttpSession session = request.getSession(false);
        if (session != null) {
            session.invalidate(); // Terminate the current session
        }
        return "redirect:/";
    }
}
