package it.unipi.dsmt.FitConnect.controller;

import it.unipi.dsmt.FitConnect.entities.ActiveCourses;
import it.unipi.dsmt.FitConnect.entities.ClassSchedules;
import it.unipi.dsmt.FitConnect.services.DBService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;

@Controller
@RequestMapping("/home")
public class PagesController {
    @Autowired
    private DBService database;
    @Autowired
    private ActiveCourses courses;  // all gym courses in the db or only those of a user?
    @Autowired
    private ClassSchedules classSchedules;  // for a single course or booked by a client or taught by a trainer

    private void loadCourses() {
        courses.clear();
        courses.addAll(database.getCourseRepository().findAll());
    }

    private void loadCourses(String user) {
        courses.clear();
        // todo: da modificare in base alla nuova struttura del document
        courses.addAll(database.getCourseRepository().findByUser(user));
    }

    private void loadTrainerCourses(String trainer) {
        courses.clear();
        courses.addAll(database.getCourseRepository().findByTrainer(trainer));
    }
    /* todo: aggiungere i vari metodi */


    @GetMapping("/home")
    public String login() {     // todo: cambiare nome metodo?
        return "home";
    }

    @GetMapping("/courses")
    public String viewCourses(Model model) {
        /* do something */
        return "home";
    }

    @GetMapping("/profile/{user}")
    public String viewUserProfile(Model model,
                                  @PathVariable String user) {
        /* do something */
        return "home";
    }

}
