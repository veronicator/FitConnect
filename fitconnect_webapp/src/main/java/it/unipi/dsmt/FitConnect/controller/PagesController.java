package it.unipi.dsmt.FitConnect.controller;

import it.unipi.dsmt.FitConnect.entities.*;
import it.unipi.dsmt.FitConnect.services.DBService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

import java.util.Optional;

@Controller
@RequestMapping("/home")
public class PagesController {
    @Autowired
    private DBService dbService;
    @Autowired
    private ActiveCourses courses;  // all gym courses in the db or only those of a user?

    // gestire il contesto per l'autenticazione, in modo da sapere sempre qual è l'utente connesso
    private void browseCourses() {
        courses.clear();
        courses.addAll(dbService.getCourseRepository().findAll());
    }

    private void loadClientCourses(String username) {
        courses.clear();
        // todo: da modificare in base alla nuova struttura del document
        // va cambiata con userRepository.findCourses() -> va dichiarato il metodo corretto
        courses.addAll(dbService.getCourseRepository().findByUser(username));
    }

    private void loadTrainerCourses(String trainer) {
        courses.clear();
        courses.addAll(dbService.getCourseRepository().findByTrainer(trainer));
    }
    /* todo: aggiungere i vari metodi */

    /** called when a client clicks on the bookedButton in the UI */
    private boolean bookClass(String scheduleId, String username) {
        /* try {
        *   find the user
        *   find the schedule
        *   if available places is <= 0
        *       => return false
        *   else
        *       => book the class adding the user in the bookedUsers list
        *       => decrement availablePlaces field
        *       => add the reservation to the user object
        *       => save the Schedule doc on the db
        * } catch(ObjectOptimisticLockingFailureException e) {
        *   return false
        * }
        * return true
        * */
        return false;
    }

    @GetMapping("/home")
    public String login() {     // todo: cambiare nome metodo?
        return "home";
    }

    @GetMapping("/courses/{courseName}")
    public String browseCourses(Model model, @PathVariable String courseName) {
        /* -> to show all courses of the same type (e.g. all yoga courses, having different trainers)
        * findAllCoursesByName(courseName)
        * -> show the list */
        return "home";
    }

    @GetMapping("/courses/{courseId}")
    public String viewCourse(Model model,
                             @PathVariable String courseId) {
        /* getCourse(courseId)
        * show course details:
        *   tab with list of scheduled classes
        * es. yoga
        *   Date       |    class time      |   available places    | book button
        *   monday 05-02-2024 17:00 -> 18:00        16      book
        *
        * the book button can be disabled if available places is <= 0
        *
        * if the logged user is a trainer => the book button is substituted with
        * an "edit" or "remove class" button
        * and another button to add new class times has to be shown
        * */
        return "home";
    }

    /** raggiungibile solo da un trainer per aggiungere un nuovo corso*/
    @PostMapping("/courses/create")
    public String addCourse(Model model,
                            @ModelAttribute(value = "course") Course course) {
        /* check that the logged user is a trainer (or even the admin ?)
        * -> retrieve the user logged through the authentication context
        * if a course with the same trainer and same name already exists (check in the db service)
        *   => error (an error message to show in the UI ?)
        * else
        *   => dbService.addCourse (use try-catch in the db method)
        *   => if course added correct
        *       => show success message/ load course page "/course/courseId"
        * */

        return "home";
    }

    // todo: metodo per aggiungere/modificare gli orari delle lezioni
    @PostMapping("/course/schedule")
    public String addSchedule(Model model, @ModelAttribute(value="schedule") Reservations reservations) {
        /* inserire parametri di funzione corretti
        * controllare che non ci sia sovrapposizione di orari (riferito solo ad uno stesso corso)
        * schedule.save
        * gestire eventuali errori*/
        return "home";
    }

    @GetMapping("/profile/{username}")
    public String viewUserProfile(Model model,
                                  @PathVariable String username) {
//        todo: create getUser method in DBService
//        MongoUser user = dbService.getUser(username);
        Optional<MongoUser> optUser = dbService.getUserRepository().findByUsername(username);
        if (optUser.isPresent()) {
            MongoUser user = optUser.get();
            switch (user.getRole()) {
                case trainer -> {
                    // todo: load trainer page, showing taught courses
                    System.out.println("trainer");
                }
                case client -> {
                    // todo: load client page, showing subscribed courses
                    // also show booked classes in a tab, with a button to can remove the reservation
                    System.out.println("client");
                }
                default -> {
                    // todo: manage error
                    System.out.println("Invalid user role");
                }
            }
            return "home";  // todo: che ritorno usare?
        }
        return "home";  // or return "error" ?
    }

}
