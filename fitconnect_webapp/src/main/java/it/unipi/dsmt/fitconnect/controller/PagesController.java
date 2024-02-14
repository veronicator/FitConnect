package it.unipi.dsmt.fitconnect.controller;

import it.unipi.dsmt.fitconnect.enums.CourseType;
import it.unipi.dsmt.fitconnect.enums.UserRole;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;
import it.unipi.dsmt.fitconnect.entities.*;
import it.unipi.dsmt.fitconnect.services.DBService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

import java.time.*;
import java.time.format.*;
import java.util.*;
import java.util.stream.*;

@Controller
public class PagesController {
    @Autowired
    private DBService dbService;
    @Autowired
    private ActiveCourses courses;  // all gym courses in the db or only those of a user?
//    private ErlangNodeController;

    @GetMapping({"/", "/index"})
    public String index() {     // todo: cambiare nome metodo?
        return "index";
    }

    // gestire il contesto per l'autenticazione, in modo da sapere sempre qual è l'utente connesso
    private void browseCourses() {
        courses.clear();
        courses.addAll(dbService.getCourseRepository().findAll());
    }

    private void loadClientCourses(String username) {
        courses.clear();
        // todo: da modificare in base alla nuova struttura del document
        // va cambiata con userRepository.findCourses() -> va dichiarato il metodo corretto
//        courses.addAll(dbService.getCourseRepository().findByUser(username));
    }

    private void loadTrainerCourses(String trainer) {
        courses.clear();
        courses.addAll(dbService.browseUserCourses(trainer));
    }
    /* todo: aggiungere i vari metodi */

    /**
     * called when a client clicks on the bookedButton in the UI
     */
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
        * [join, coursename]
        * [deleteC
        * String command = "join-Yoga"
        * erlangNodeController.sendCommandToNode(username, command)
        * } catch(ObjectOptimisticLockingFailureException e) {
        *   return false
        * }
        * return true
        * */
        return false;
    }

    @GetMapping("/home")
    public String home(HttpServletRequest request, Model model) {
        HttpSession session = request.getSession(false); // non creare una nuova sessione se non esiste già

        if (session != null) {
            String username = (String) session.getAttribute("username");
            UserRole role = (UserRole) session.getAttribute("role");


            if (role != null && role == UserRole.client) {
                model.addAttribute("username", username);
                return "home";
            }
        }

        return "redirect:/login"; // Reindirizza alla pagina di login se l'utente non è autenticato o non ha il ruolo appropriato
    }


//    @GetMapping("/courses/{courseName}")
//    public String browseCourses(Model model, @PathVariable String courseName) {
//        /* -> to show all courses of the same type (e.g. all yoga courses, having different trainers)
//        * findAllCoursesByName(courseName)
//        * -> show the list */
//        return "home";
//    }


    @GetMapping("/courses")
    public String courses(Model model) {
        List<Course> courses = dbService.getCourseRepository().findAll();
        /*Collections.sort(courses, Comparator.comparing(Course::getCourseName));

        Set<String> uniqueCourseNames = courses.stream()
                .map(Course::getCourseName)
                .collect(Collectors.toSet());

//        model.addAttribute("courseName", uniqueCourseNames);

         */
        // todo: check
        model.addAttribute("courseName", CourseType.values());
        model.addAttribute("courseList", courses);

        return "courses";
    }

    @GetMapping("/courses/{course}/{trainer}")
    public String viewCourseSchedule(@PathVariable String course, @PathVariable String trainer,
                                     HttpServletRequest request, Model model) {

        Course trainerCourse = dbService.getCourseRepository().findByTrainerUsername(trainer).get(0);
        List<ClassTime> weekSchedule = trainerCourse.getWeekSchedule();

        List<Course> courses = dbService.getCourseRepository().findAll();
        /*Collections.sort(courses, Comparator.comparing(Course::getCourseName));

        Set<String> uniqueCourseNames = courses.stream()
                .map(Course::getCourseName)
                .collect(Collectors.toSet());

         */
        boolean isJoined = false;
        // check if logged user is subscribed to this course
        HttpSession session = request.getSession(false);
        if(session != null) {
            MongoUser loggedUser = (MongoUser) session.getAttribute("loggedUser");
            System.out.println(trainerCourse.getEnrolledClients());
            isJoined = trainerCourse.getEnrolledClients().contains(loggedUser.getId());

        }

        model.addAttribute("isJoined", isJoined);
//        model.addAttribute("courseName", uniqueCourseNames);
        model.addAttribute("courseName", CourseType.values());
        model.addAttribute("courseList", courses);
        model.addAttribute("weekSchedule", weekSchedule);
        model.addAttribute("trainer", trainer);

        return "courses";
    }

    @GetMapping("/courses/{course}")
    public String viewCourse(Model model,
                             @PathVariable String course) {


        List<Course> courses = dbService.getCourseRepository().findByCourseName(CourseType.valueOf(course));


        List<ClassTime> weekSchedule = courses.get(0).getWeekSchedule();


        model.addAttribute("courseName", course);
        model.addAttribute("courseList", courses);
        model.addAttribute("weekSchedule", weekSchedule);


        return "courses";

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

    }

    @PostMapping("/joinCourse")
    public String joinCourse(){

        // aggiungere servizio per joinare al corso

        return "courses";
    }

    @GetMapping("/add-class-time")
    public String addCourse(HttpServletRequest request, Model model) {
        HttpSession session = request.getSession(false); // non creare una nuova sessione se non esiste già

        if (session != null) {
            String username = (String) session.getAttribute("username");
            UserRole role = (UserRole) session.getAttribute("role");


            if (role != null && role == UserRole.trainer) {
                model.addAttribute("classTime", new ClassTime());
                return "addClassTime";
            }
        }

        return "redirect:/";
    }

    @PostMapping("/add-class-time")
    public String doAddCourse(@ModelAttribute ClassTime classTime) {

        String courseId = "65bead220356226c852ba311";

        dbService.addClassTime(courseId, classTime.getDayOfWeek(), classTime.getStartTime(), classTime.getEndTime());

        return "courses";
    }

    @PostMapping("/bookClass")
    public String bookClass(@RequestParam String trainer,
                            @RequestParam String day,
                            @RequestParam String startTime, Model model) {
        String courseId = "boh";

        // Converti la stringa di startTime in un oggetto LocalTime
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("HH:mm");
        LocalTime start = LocalTime.parse(startTime, formatter);

        // Aggiungi un'ora all'orario di inizio per ottenere l'orario di fine
        LocalTime end = start.plusHours(1);

        boolean ret = dbService.addClassTime(courseId, DayOfWeek.valueOf(day), start, end);

        if (ret)
            return "courses";
        else
            return "index";
    }

//    @GetMapping("/courses/{courseId}")
//    public String viewCoursebyId(Model model,
//                             @PathVariable String courseId) {
//        /* getCourse(courseId)
//        * show course details:
//        *   tab with list of scheduled classes
//        * es. yoga
//        *   Date       |    class time      |   available places    | book button
//        *   monday 05-02-2024 17:00 -> 18:00        16      book
//        *
//        * the book button can be disabled if available places is <= 0
//        *
//        * if the logged user is a trainer => the book button is substituted with
//        * an "edit" or "remove class" button
//        * and another button to add new class times has to be shown
//        * */
//        return "home";
//    }

    /**
     * raggiungibile solo da un trainer per aggiungere un nuovo corso
     */
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
    public String addSchedule(Model model, @ModelAttribute(value = "schedule") Reservations reservations) {
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

    private void populate_courses() {
        // Ripopolamento della collection "courses"
        /*
        dbService.addCourse("yoga", "Mario Rossi");
        dbService.addCourse("yoga", "Giulia Bianchi");
        dbService.addCourse("yoga", "Luca Verdi");
        dbService.addCourse("yoga", "Anna Esposito");
        dbService.addCourse("yoga", "Matteo Russo");
        dbService.addCourse("yoga", "Sara Ferrari");
        dbService.addCourse("yoga", "Giovanni Romano");
        dbService.addCourse("yoga", "Chiara Gialli");
        dbService.addCourse("yoga", "Andrea Neri");
        dbService.addCourse("yoga", "Elena Marroni");

        dbService.addCourse("crossfit", "Alessandro Moretti");
        dbService.addCourse("crossfit", "Valentina Rosso");
        dbService.addCourse("crossfit", "Davide Rossi");
        dbService.addCourse("crossfit", "Martina Verdi");
        dbService.addCourse("crossfit", "Roberto Bianchi");
        dbService.addCourse("crossfit", "Francesca Neri");
        dbService.addCourse("crossfit", "Paolo Esposito");
        dbService.addCourse("crossfit", "Giorgio Gialli");
        dbService.addCourse("crossfit", "Eleonora Marroni");
        dbService.addCourse("crossfit", "Simone Ferrari");

        dbService.addCourse("swim", "Federico Bianchi");
        dbService.addCourse("swim", "Laura Verdi");
        dbService.addCourse("swim", "Marco Rossi");
        dbService.addCourse("swim", "Alessia Esposito");
        dbService.addCourse("swim", "Nicola Neri");
        dbService.addCourse("swim", "Cristina Gialli");
        dbService.addCourse("swim", "Lorenzo Marroni");
        dbService.addCourse("swim", "Erika Ferrari");
        dbService.addCourse("swim", "Daniele Romano");
        dbService.addCourse("swim", "Valeria Rosso");

        dbService.addCourse("gym", "Gabriele Verdi");
        dbService.addCourse("gym", "Elisa Rossi");
        dbService.addCourse("gym", "Massimo Bianchi");
        dbService.addCourse("gym", "Serena Esposito");
        dbService.addCourse("gym", "Riccardo Neri");
        dbService.addCourse("gym", "Alessandra Gialli");
        dbService.addCourse("gym", "Giacomo Marroni");
        dbService.addCourse("gym", "Chiara Ferrari");
        dbService.addCourse("gym", "Luigi Romano");
        dbService.addCourse("gym", "Martina Rosso");

         */
    }

}
