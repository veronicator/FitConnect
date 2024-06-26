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
        if (session == null) {
            return null;
        }
        return (String) session.getAttribute("username");
    }

    private UserRole getSessionRole(HttpSession session) {
        return (UserRole) session.getAttribute("role");
    }

    private void loadCourses() {
        courses.clear();
        courses.addAll(dbService.browseAllCourses());
    }

    private void loadCourses(String course) {
        courses.clear();
        courses.addAll(dbService.browseCourses(course));
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

        String trainerUsername = courses.get(0).getTrainerUsername();

        model.addAttribute("courseName", course);
        model.addAttribute("courseList", courses);

        return "redirect:/courses/" + course + "/" + trainerUsername;

    }

    @GetMapping("/courses/{course}/{trainer}")
    public String viewCourseSchedule(@PathVariable String course, @PathVariable String trainer,
                                     HttpServletRequest request, Model model) {

        loadCourses(course);

        Course trainerCourse = dbService.getByCourseAndTrainer(course, trainer);

        if (trainerCourse == null) {
            String errorMessage = "There is no trainer " + trainer + " with course " + course;
            System.out.println(errorMessage);
            model.addAttribute("errorMessage", errorMessage);
            return "error";
        }

        // check if logged user is subscribed to this course
        boolean isJoined = false;

        HttpSession session = request.getSession(false);
        if (session != null) {
            String username = getSessionUsername(session);
            model.addAttribute("username", getSessionUsername(session));
            List<MongoUser> enrolledClients = trainerCourse.getEnrolledClients();
            // Check if there is a MongoUser with the same username in the list of subscribed users
            isJoined = enrolledClients.stream().anyMatch(user -> user.getUsername().equals(username));
            System.out.println("isJoind = " + isJoined);
        }

        List<ClassTime> weekSchedule = trainerCourse.getWeekSchedule();

        List<DayOfWeek> days = new ArrayList<>(Arrays.stream(DayOfWeek.values()).toList());
        days.remove(DayOfWeek.SUNDAY);

        model.addAttribute("isJoined", isJoined);

        model.addAttribute("trainerCourse", trainerCourse);
        model.addAttribute("weekSchedule", weekSchedule);
        model.addAttribute("trainer", trainer);

        model.addAttribute("courseName", course);
        model.addAttribute("courseList", courses);

        model.addAttribute("daysOfWeek", days);

        return "courseType";
    }

    @PostMapping("/courses/{course}/{trainer}/{courseId}/joinCourse")
    public String joinCourse(@PathVariable String course, @PathVariable String trainer, @PathVariable String courseId,
                             HttpServletRequest request, Model model) {

        HttpSession session = request.getSession(false);

        if (session == null) {
            String errorMessage = "Session error, please login";
            System.out.println(errorMessage);
            return "login";
        }

        String username = getSessionUsername(session);

        if (dbService.joinCourse(courseId, username)) {
            String joinCommand = String.format("join-%s", courseId);
            erlangNodesController.sendCommandToNode(username, joinCommand);
            System.out.println(username + " subscribed correctly!");
            return "redirect:/courses/" + course + "/" + trainer;
        } else {
            String errorMessage = "subscription failed: retry";
            System.out.println(errorMessage);
            model.addAttribute("errorMessage", errorMessage);
            return "error";
        }
    }

    @PostMapping("/courses/{course}/{trainer}/{courseId}/bookClass")
    public String bookClass(@PathVariable String course, @PathVariable String trainer, @PathVariable String courseId,
                            @RequestParam String day, @RequestParam String startTime,
                            Model model, HttpServletRequest request) {

        HttpSession session = request.getSession(false);
        if (session == null) {
            String errorMessage = "Session error";
            System.out.println(errorMessage);
            model.addAttribute("errorMessage", errorMessage);
            return "error";
        }

        String username = getSessionUsername(session);

        Reservations reservations = dbService.bookClass(username, courseId, DayOfWeek.valueOf(day), startTime);

        if (reservations != null) {
            String bookingCommand = String.format("bookClass-%s-%d",
                    reservations.getId().toString(),
                    reservations.getActualClassTime().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli());
            erlangNodesController.sendCommandToNode(username, bookingCommand);

            return "redirect:/profile?view=reservations";

        } else {
            String errorMessage = "Book class failed: retry";
            System.out.println(errorMessage);
            model.addAttribute("errorMessage", errorMessage);
            return "error";
        }

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

        MongoUser user = dbService.getUser(username);
        if (user != null) {
            switch (user.getRole()) {
                case trainer -> {
                    model.addAttribute("courses", user.getCourses());
                    if (course != null) {
                        Course courseObj = dbService.getByCourseAndTrainer(course, username);
                        if (courseObj != null) {

                            model.addAttribute("courseName", courseObj.getCourseName());
                            model.addAttribute("classes", courseObj.getWeekSchedule());
                            model.addAttribute("courseId", courseObj.getId());
                        }
                    }
                }
                case client -> {
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

        String newCourseId = dbService.addNewCourse(CourseType.valueOf(course), username, maxReservablePlaces);
        if (newCourseId != null) {
            String joinCommand = String.format("join-%s", newCourseId);
            erlangNodesController.sendCommandToNode(username, joinCommand);

            return "redirect:/profile";
        }
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

        LocalTime startTimeLocal = LocalTime.parse(startTime);

        // Compute endTime adding one hour to startTime
        LocalTime endTimeLocal = startTimeLocal.plus(Duration.ofHours(1));

        Course course = dbService.addClassTime(courseId, DayOfWeek.valueOf(day), startTimeLocal, endTimeLocal);
        if (course != null) {
            System.out.println("class added");

            return "redirect:/courses/" + course.getCourseName() + '/' + course.getTrainerUsername();
        } else
            return "error";
    }

    @PostMapping("/deleteCourse")
    public String deleteCourse(@RequestParam String courseId) {

        Course course = dbService.getCourse(courseId);
        if (course == null)
            return "error";

        List<MongoUser> users = course.getEnrolledClients();
        for (MongoUser u: users) {
            String leaveCommand = String.format("leave-%s", course.getId().toString());
            erlangNodesController.sendCommandToNode(u.getUsername(), leaveCommand);
        }

        List<Reservations> reservations = dbService.getReservationsByCourse(courseId);
        for (Reservations r: reservations) {
            for (MongoUser u: r.getBookedUsers()) {
                String unbookCommand = String.format("unbookClass-%s",
                        r.getId().toString());
                erlangNodesController.sendCommandToNode(u.getUsername(), unbookCommand);
            }
        }
        String leaveCommand = String.format("leave-%s", course.getId().toString());
        erlangNodesController.sendCommandToNode(course.getTrainerUsername(), leaveCommand);

        if (dbService.deleteCourse(courseId)) {

            System.out.println("/deleteCourse ok");
            return "redirect:/profile";
        } else
            return "error";
    }

    @PostMapping("/unsubscribeCourse")
    public String unsubscribeCourse(@RequestParam String courseId, HttpServletRequest request) {

        HttpSession session = request.getSession(false);
        if (session == null) {
            String errorMessage = "Session error";
            System.out.println(errorMessage);
            return "error";
        }

        String username = getSessionUsername(session);

        List<Reservations> clientReservations = dbService.getReservationsByCourseAndUser(courseId, username);
        
        for (Reservations r: clientReservations) {
            String unbookCommand = String.format("unbookClass-%s", r.getId().toString());
            erlangNodesController.sendCommandToNode(username, unbookCommand);
        }
        
        String leaveCommand = String.format("leave-%s", courseId);
        erlangNodesController.sendCommandToNode(username, leaveCommand);

        if (dbService.leaveCourse(courseId, username)) {
            System.out.println("course left");
            return "redirect:/profile";
        }

        return "error";
    }

    @PostMapping("/deleteClass")
    public String deleteClass(@RequestParam String courseId, @RequestParam String day, @RequestParam String startTime) {

        LocalTime startTimeLocal = LocalTime.parse(startTime);

        List<Reservations> reservations = dbService.removeClassTime(courseId, DayOfWeek.valueOf(day), startTimeLocal);
        if (reservations != null) {
            for (Reservations r: reservations) {
                for (MongoUser u: r.getBookedUsers()) {
                    String unbookCommand = String.format("unbookClass-%s",
                            r.getId().toString());
                    erlangNodesController.sendCommandToNode(u.getUsername(), unbookCommand);
                }
            }
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

        List<Reservations> reservationsUpdated = dbService.editCourseClassTime(courseId, DayOfWeek.valueOf(oldDay),
                oldStartTimeLocal, DayOfWeek.valueOf(newDay), newStartTimeLocal, newEndTimeLocal);
        if (reservationsUpdated != null) {
            for (Reservations r: reservationsUpdated) {
                for (MongoUser u: r.getBookedUsers()) {
                    String editCommand = String.format("editClass-%s-%d",
                            r.getId().toString(),
                            r.getActualClassTime().atZone(ZoneId.systemDefault()).toInstant().toEpochMilli());
                    erlangNodesController.sendCommandToNode(u.getUsername(), editCommand);
                }
            }
            System.out.println("class time edited");
            return "redirect:/profile";
        } else
            return "error";
    }

    @PostMapping("/unbookClass")
    public String unbookClass(HttpServletRequest request, @RequestParam String classId) {

        HttpSession session = request.getSession(false);
        if (session == null) {
            System.out.println("No session");
            return "login";
        }
        System.out.println("class id: " + classId);
        String username = getSessionUsername(session);

        String reservationId = dbService.unbookClass(classId, username);

        if (reservationId != null) {
            String unbookCommand = String.format("unbookClass-%s",
                    reservationId);
            erlangNodesController.sendCommandToNode(username, unbookCommand);
            return "redirect:/profile?view=reservations";
        }

        return "error";
    }

    @GetMapping("/logout")
    public String logout(HttpServletRequest request) {
        HttpSession session = request.getSession(false);
        if (session != null) {
            String username = getSessionUsername(session);

            session.invalidate(); // Terminate the current session

            erlangNodesController.disconnectNode(username);
        }
        return "redirect:/?isLogout=true";
    }

    @GetMapping("/chat")
    public String chat(HttpServletRequest request, Model model){

        HttpSession session = request.getSession(false);
        if (session == null) {
            System.out.println("No session");
            return "login";
        }

        String username = getSessionUsername(session);

        List<Course> chatCourses = dbService.getUser(username).getCourses();

        return "redirect:/chat/" + chatCourses.get(0).getId();
    }

    @GetMapping("/chat/{room}")
    public String chat(@PathVariable String room, @RequestParam(name = "pageNumber", defaultValue = "0") int pageNumber,
                       HttpServletRequest request, Model model){

        HttpSession session = request.getSession(false);
        if (session == null) {
            System.out.println("No session");
            return "login";
        }

        String username = getSessionUsername(session);

        List<Course> chatCourses = dbService.getUser(username).getCourses();
        model.addAttribute("chatCourses", chatCourses);

        List<Message> chatMessages = dbService.getCourseMessages(username, room, pageNumber);

        model.addAttribute("chatMessages", chatMessages);
        model.addAttribute("course", dbService.getCourse(room));
        model.addAttribute("room", room);
        model.addAttribute("pageNumber", pageNumber);

        return "chat";
    }

    @GetMapping("/chat/{room}/{page}")
    @ResponseBody
    public List<Message> chatPageable(@PathVariable String room, @PathVariable int page,
                                     HttpServletRequest request){

        HttpSession session = request.getSession(false);
        String username = getSessionUsername(session);

        List<Message> chatMessages = dbService.getCourseMessages(username, room, page);
        return chatMessages;
    }



}


