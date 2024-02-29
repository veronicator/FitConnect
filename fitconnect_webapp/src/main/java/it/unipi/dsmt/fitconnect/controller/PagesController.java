package it.unipi.dsmt.fitconnect.controller;

import it.unipi.dsmt.fitconnect.enums.CourseType;
import it.unipi.dsmt.fitconnect.enums.UserRole;
import it.unipi.dsmt.fitconnect.erlang.ErlangNodesController;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpSession;
import it.unipi.dsmt.fitconnect.entities.*;
import it.unipi.dsmt.fitconnect.services.DBService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

import java.time.*;
import java.util.*;

@Controller
public class PagesController {
    @Autowired
    private DBService dbService;
    @Autowired
    private ErlangNodesController erlangNodesController;
    @Autowired
    private ActiveCourses courses;  // all gym courses in the db or only those of a user, update everytime

    private String getSessionUsername(HttpSession session) {
        return (String) session.getAttribute("username");
    }

    private UserRole getSessionRole(HttpSession session) {
        return (UserRole) session.getAttribute("role");
    }

    // gestire il contesto per l'autenticazione, in modo da sapere sempre qual è l'utente connesso
    private void loadCourses() {
        courses.clear();
        courses.addAll(dbService.browseAllCourses());
    }

    private void loadClientCourses(String username) {
        courses.clear();
        courses.addAll(dbService.browseUserCourses(username));
    }

    private void loadTrainerCourses(String trainer) {
        courses.clear();
        courses.addAll(dbService.browseUserCourses(trainer));
    }

    @GetMapping({"/", "/index"})
    public String index(@RequestParam(required = false) String isLogout, Model model) {
        model.addAttribute("isLogout", isLogout != null && isLogout.equals("true"));
        return "index";
    }

    @GetMapping("/error")
    public String error() {
        return "error";
    }

    @GetMapping("/courses")
    public String courses(Model model) {
        loadCourses();

        model.addAttribute("courseNames", CourseType.values());

        return "courses";
    }

    @GetMapping("/courses/{course}")
    public String viewCourse(@PathVariable String course, Model model) {

        courses.clear();
        courses.addAll(dbService.browseCourses(course));

        model.addAttribute("courseName", course);
        model.addAttribute("courseList", courses);

        return "courseType";
    }

    @GetMapping("/courses/{course}/{trainer}")
    public String viewCourseSchedule(@PathVariable String course, @PathVariable String trainer,
                                     HttpServletRequest request, Model model) {

        Course trainerCourse = dbService.getByCourseAndTrainer(CourseType.valueOf(course), trainer);

        if (trainerCourse == null) {
            String errorMessage = "There is no trainer " + trainer + " with course " + course;
            System.out.println(errorMessage);
            return "error";
        }

        // check if logged user is subscribed to this course
        boolean isJoined = false;

        HttpSession session = request.getSession(false);
        if (session != null) {
            String username = getSessionUsername(session);
            List<MongoUser> enrolledClients = trainerCourse.getEnrolledClients();
            // Controllare se c'è un MongoUser con lo stesso username nella lista degli utenti iscritti
            isJoined = enrolledClients.stream().anyMatch(user -> user.getUsername().equals(username));
        }

        //todo sortare correttamente le liste
        List<ClassTime> weekSchedule = trainerCourse.getWeekSchedule();
        Set<LocalTime> uniqueStartTimes = new HashSet<>();
        Set<DayOfWeek> uniqueDaysOfWeek = new HashSet<>();

//        if(!weekSchedule.isEmpty())
//            for (ClassTime classtime : weekSchedule) {
//                uniqueStartTimes.add(classtime.getStartTime());
//                uniqueDaysOfWeek.add(classtime.getDayOfWeek());
//            }
//
//        // Ordinamento dei startTime
//        List<LocalTime> sortedStartTimes = new ArrayList<>(uniqueStartTimes);
//        sortedStartTimes.sort(Comparator.naturalOrder());
//
//// Ordinamento dei daysOfWeek
//        List<DayOfWeek> sortedDaysOfWeek = new ArrayList<>(uniqueDaysOfWeek);
//        sortedDaysOfWeek.sort(Comparator.comparingInt(DayOfWeek::getValue));
//
//// Se desideri mantenere ancora un Set, puoi convertire la lista ordinata in un Set
//        Set<LocalTime> sortedUniqueStartTimes = new HashSet<>(sortedStartTimes);
//        Set<DayOfWeek> sortedUniqueDaysOfWeek = new HashSet<>(sortedDaysOfWeek);

        List<DayOfWeek> days = new ArrayList<>(Arrays.stream(DayOfWeek.values()).toList());
        days.remove(DayOfWeek.SUNDAY);

        model.addAttribute("isJoined", isJoined);
        model.addAttribute("courseList", courses);
        model.addAttribute("trainerCourse", trainerCourse);
        model.addAttribute("weekSchedule", weekSchedule);
        model.addAttribute("trainer", trainer);
        model.addAttribute("username", getSessionUsername(session));
        model.addAttribute("courseName", course);
        model.addAttribute("daysOfWeek", days);
//        model.addAttribute("bookDay", sortedUniqueDaysOfWeek);
//        model.addAttribute("bookTime", sortedUniqueStartTimes);

        return "courseType";
    }

    @PostMapping("/courses/{course}/{trainer}/{courseId}/joinCourse")
    public String joinCourse(@PathVariable String course, @PathVariable String trainer, @PathVariable String courseId, HttpServletRequest request, Model model) {

        HttpSession session = request.getSession(false);
        System.out.println(session);
        if (session == null) {
            String errorMessage = "Session error";
            System.out.println(errorMessage);
            return "error";
        }
        String username = getSessionUsername(session);
        if (dbService.joinCourse(courseId, username))
            System.out.println(username + " subscribed correctly!");
        else
            System.out.println("subscription failed: retry");
        // todo: sistemare check return value

        return "redirect:/courses/" + course + "/" + trainer;
    }

    @PostMapping("/courses/{course}/{trainer}/{courseId}/bookClass")
    public String bookClass(@PathVariable String course, @PathVariable String courseId, @PathVariable String trainer,
                            @RequestParam String day, @RequestParam String startTime, Model model, HttpServletRequest request) {

        HttpSession session = request.getSession(false);
        if (session == null) {
            String errorMessage = "Session error";
            System.out.println(errorMessage);
            return "error";
        }

        String username = getSessionUsername(session);

        boolean ret = dbService.bookClass(username, courseId, DayOfWeek.valueOf(day), startTime);

        // todo parte erlang

        if (ret)
            return "redirect:/profile";
        else
            return "error";
    }

    @GetMapping("/profile")
    public String profile(HttpServletRequest request, Model model,
                          @RequestParam(name = "view", defaultValue = "courses") String view,
                          @RequestParam(name = "course", required = false) String course) {

        HttpSession session = request.getSession(false);
        if (session == null) {
            String errorMessage = "No session found";
            System.out.println(errorMessage);
            return "login";
        }

        String username = getSessionUsername(session);
        UserRole userRole = getSessionRole(session);

        MongoUser user = dbService.getUser(username);
        if (user != null) {
            switch (user.getRole()) {
                case trainer -> {
                    model.addAttribute("courses", user.getCourses());
                    if (course != null) {
                        String trainerUsername = user.getCompleteName();
                        Course courseObj = dbService.getByCourseAndTrainer(CourseType.valueOf(course), trainerUsername);
                        if (courseObj != null) {
//                    courseOpt.ifPresent(value -> model.addAttribute("courseName", value.getCourseName()));
                            model.addAttribute("courseName", courseObj.getCourseName());
                            model.addAttribute("classes", courseObj.getWeekSchedule());
                            model.addAttribute("courseId", courseObj.getId());
                        }
                    }
                }
                case client -> {
                    // todo: load client page, showing subscribed courses
                    // also show booked classes in a tab, with a button to can remove the reservation
                    model.addAttribute("courses", user.getCourses());
                    model.addAttribute("reservations", user.getReservations());
                }
                default -> {
                    System.out.println("Invalid user role");
                    return "error";
                }
            }
            model.addAttribute("view", view);
            return "profile";
        } else
            return "error";
    }

    @GetMapping("/addCourse")
    public String addCourse(HttpServletRequest request, Model model) {

        HttpSession session = request.getSession(false);
        if (session == null) {
            String errorMessage = "Session error";
            System.out.println(errorMessage);
            return "redirect:/login";
        }
        // only trainer can access to this page
        if (!getSessionRole(session).toString().equals("trainer")) {
            String errorMessage = "Content not accessible";
            System.out.println(errorMessage);
            return "error";
        }

        model.addAttribute("courses", CourseType.values());

        return "addCourse";
    }

    @PostMapping("/addCourse")
    public String doAddCourse(@RequestParam String course,
                              @RequestParam Integer maxReservablePlaces, HttpServletRequest request) {

        HttpSession session = request.getSession(false);
        if (session == null) {
            String errorMessage = "Session error";
            System.out.println(errorMessage);
            return "redirect:/login";
        }
        // only trainer can access to this page
        if (!getSessionRole(session).toString().equals("trainer")) {
            String errorMessage = "Content not accessible";
            System.out.println(errorMessage);
            return "error";
        }

        String username = getSessionUsername(session);

        boolean ret = dbService.addNewCourse(CourseType.valueOf(course), username, maxReservablePlaces);
        if (ret)
            return "redirect:/profile";
        else
            return "error";
    }


    @GetMapping("/addClass")
    public String addClass(HttpServletRequest request, Model model) {

        HttpSession session = request.getSession(false);
        if (session == null) {
            String errorMessage = "Session error";
            System.out.println(errorMessage);
            return "redirect:/login";
        }
        // only trainer can access to this page
        if (!getSessionRole(session).toString().equals("trainer")) {
            String errorMessage = "Content not accessible";
            System.out.println(errorMessage);
            return "error";
        }

        List<DayOfWeek> days = new ArrayList<>(Arrays.stream(DayOfWeek.values()).toList());
        days.remove(DayOfWeek.SUNDAY);

        model.addAttribute("classTime", new ClassTime());
        model.addAttribute("daysOfWeek", days);
        return "addClassTime";

    }

    @PostMapping("/addClass/{courseId}")
    public String doAddClass(@PathVariable String courseId, @RequestParam String day, @RequestParam String startTime) {

        // Converti il startTime da stringa a LocalTime
        LocalTime startTimeLocal = LocalTime.parse(startTime);

        // Calcola l'endTime aggiungendo un'ora al startTime
        LocalTime endTimeLocal = startTimeLocal.plus(Duration.ofHours(1));

        boolean ret = dbService.addClassTime(courseId, DayOfWeek.valueOf(day), startTimeLocal, endTimeLocal);
        if (ret) {
            System.out.println("class added");
            return "redirect:/profile";
        } else
            return "index";
    }

    @PostMapping("/deleteCourse")
    public String deleteCourse(@RequestParam String courseId) {

        boolean ret = dbService.deleteCourse(courseId);
        if (ret) {
            System.out.println("course removed");
            return "redirect:/profile";
        } else
            return "error";
    }

    @PostMapping("/unsubscribeCourse")
    public String unsubscribeCourse(@RequestParam String courseId, HttpServletRequest request) {

        HttpSession session = request.getSession(false);
        if(session == null){
            String errorMessage = "Session error";
            System.out.println(errorMessage);
            return "error";
        }

        String username = getSessionUsername(session);

        boolean ret = dbService.leaveCourse(courseId, username);
        if (ret) {
            return "redirect:/profile";
        } else
            return "error";
    }

    @PostMapping("/deleteClass")
    public String deleteClass(@RequestParam String courseId, @RequestParam String day, @RequestParam String startTime) {

        LocalTime startTimeLocal = LocalTime.parse(startTime);

        boolean ret = dbService.removeClassTime(courseId, DayOfWeek.valueOf(day), startTimeLocal);
        if (ret) {
            return "redirect:/profile";
        } else
            return "error";
    }

    @PostMapping("/editClass")
    public String editClass(@RequestParam String courseId, @RequestParam String oldDay, @RequestParam String newDay,
                            @RequestParam String oldStartTime, @RequestParam String newStartTime) {

        LocalTime oldStartTimeLocal = LocalTime.parse(oldStartTime);
        LocalTime newStartTimeLocal = LocalTime.parse(newStartTime);
        LocalTime newEndTimeLocal = newStartTimeLocal.plus(Duration.ofHours(1));

        boolean ret = dbService.editCourseClassTime(courseId, DayOfWeek.valueOf(oldDay), oldStartTimeLocal, DayOfWeek.valueOf(newDay), newStartTimeLocal, newEndTimeLocal);
        if (ret) {
            System.out.println("class time edited");
            return "redirect:/profile";
        } else
            return "error";
    }



}


