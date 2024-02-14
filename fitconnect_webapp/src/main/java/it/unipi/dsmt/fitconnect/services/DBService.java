package it.unipi.dsmt.fitconnect.services;

import it.unipi.dsmt.fitconnect.entities.*;
import it.unipi.dsmt.fitconnect.enums.CourseType;
import it.unipi.dsmt.fitconnect.enums.UserRole;
import it.unipi.dsmt.fitconnect.repositories.mongo.*;
import lombok.Getter;
import org.bson.types.ObjectId;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.OptimisticLockingFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.temporal.TemporalAdjusters;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

@Getter
@Service
public class DBService {
    @Autowired
    private MongoUserRepository userRepository;
    @Autowired
    private CourseRepository courseRepository;
    @Autowired
    private ReservationsRepository reservationsRepository;
    @Autowired
    private MessageRepositories messageRepositories;

    //    @Scheduled(cron = "@midnight")
    @Scheduled(cron = "0 */1 * * * *")  // for testing: scheduled every minute
    public void updateReservationsCollection() {
        try {
            for (Reservations r: reservationsRepository.findPastReservations(LocalDateTime.now())){
                reservationsRepository.insert(
                        new Reservations(r.getCourse(), r.getActualClassTime().plusDays(14),
                                r.getDayOfWeek(), r.getStartTime(), r.getEndTime(), r.getMaxPlaces()));
            }
            reservationsRepository.deletePastReservations(LocalDateTime.now());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public LocalDateTime getDatetimeFromDayAndTime(DayOfWeek dayOfWeek, LocalTime atTime) {
        LocalDate nextWeekClass = LocalDate.now().with(TemporalAdjusters.next(dayOfWeek));
        return nextWeekClass.atTime(atTime);
    }

    /* usare l'username o l'id del trainer? */
    public boolean addNewCourse(CourseType courseName, String trainerUsername, Integer maxReservablePlaces) {
        try {
            Optional<MongoUser> optUser = userRepository.findByUsername(trainerUsername);
            if (optUser.isEmpty()) {
                System.out.println("addCourse failed: user not found");
                return false;
            }
            MongoUser trainer = optUser.get();
            if ((trainer.getRole().compareTo(UserRole.trainer) != 0)) {
                System.out.println("addCourse failed: logged user does not have permissions");
                return false;
            }
            if (courseRepository.existsByCourseNameAndTrainer(courseName, trainerUsername)) {
                System.out.println("same course with same trainer already exists");
                return false;
            }

            Course newCourse = new Course(courseName, trainer.getCompleteName(), trainer.getUsername(), maxReservablePlaces);
            newCourse = courseRepository.insert(newCourse);
            trainer.addCourse(newCourse);
            userRepository.save(trainer);
            System.out.println("new course added: " + newCourse.getCourseName());
        } catch (Exception e) {
            System.out.println("addCourse failed");
            e.printStackTrace();
            return false;
        }
        return true;
    }

    /** add new classTime for a course schedule
     * moreover, also Reservations documents in the db are created for the next 2 weeks
     * e.g. if the specified dayOfWeek is Monday, the document for the next Monday and
     * the following Monday are added
     * In this way, class reservations are allowed for the next 2 weeks */
    public boolean addClassTime(String courseId, DayOfWeek dayOfWeek, LocalTime startTime, LocalTime endTime) {
        try {
            if (startTime.isAfter(endTime)) {
                System.out.println("Error: startTime can not be after endTime");
                return false;
            }
            Optional<Course> optCourse = courseRepository.findById(courseId);
            if (optCourse.isEmpty()) {
                System.out.println("addClassTime failed: course not found");
                return false;
            }
            Course course = optCourse.get();
            ClassTime newClassTime = new ClassTime(dayOfWeek, startTime, endTime);
            if (course.classScheduled(newClassTime)) {
                System.out.println("class already scheduled at the requested time");
                return false;
            }
            if (course.addNewClass(newClassTime)) {
                LocalDateTime actualTime = getDatetimeFromDayAndTime(dayOfWeek, startTime);
                course = courseRepository.save(course);
                // next week reservations document
                reservationsRepository.insert(
                        new Reservations(
                                course, actualTime, dayOfWeek, startTime, endTime,
                                course.getMaxReservablePlaces()
                        )
                );
                // next 2 week reservations document
                reservationsRepository.insert(
                        new Reservations(
                                course, actualTime.plusDays(7), dayOfWeek, startTime, endTime,
                                course.getMaxReservablePlaces()
                        )
                );
            }
        } catch (OptimisticLockingFailureException e) {
            e.printStackTrace();
            return false;
        }
        return true;
    }

    public boolean joinCourse(String courseId, String username) {
        try {
            Optional<MongoUser> optUser = userRepository.findByUsername(username);
            if (optUser.isEmpty()) {
                System.out.println("joinCourse failed: user not found");
                return false;
            }
            MongoUser client = optUser.get();
            if ((client.getRole().compareTo(UserRole.client) != 0)) {
                System.out.println("joinCourse failed: logged user it's not a client");
                return false;
            }

            Optional<Course> optCourse = courseRepository.findById(courseId);
            if (optCourse.isEmpty()) {
                System.out.println("joinCourse failed: course not found");
                return false;
            }
            Course course = optCourse.get();
            if (client.addCourse(course)) {
                userRepository.save(client);
                System.out.println("JoinCourse succeeded for the course: " + course.getCourseName() +
                        " taught by: " + course.getTrainer());
                return true;
            }
        } catch (Exception e) {
            System.out.println("joinCourse failed");
            e.printStackTrace();
            return false;
        }
        return false;
    }

    public boolean bookClass(String username, String courseId, DayOfWeek dayOfWeek, LocalTime startTime) {
        try {
            Optional<MongoUser> optUser = userRepository.findByUsername(username);
            if (optUser.isEmpty()) {
                System.out.println("Booking failed: user not found.");
                return false;
            }
            MongoUser user = optUser.get();

            List<Reservations> availableClasses = reservationsRepository.findByCourseDayTime(
                    new ObjectId(courseId), dayOfWeek, startTime.toString());
            if (availableClasses.isEmpty()) {
                System.out.println("Booking failed: no available courses for the day-time selected found");
                return false;
            }

            Reservations reservations = availableClasses.get(0);
            if (reservations.getActualClassTime().isBefore(LocalDateTime.now())) {
                if (availableClasses.size() > 1)
                    reservations = availableClasses.get(1);
                else {
                    System.out.println("Booking not possible: class already held\n" +
                            "Try to book tomorrow");
                    return false;
                }
            }

            if (reservations.addBooking(user)) {
                reservations = reservationsRepository.save(reservations);
                System.out.println("class booking made for the day " + reservations.getClassDate() +
                        ", at " + reservations.getStartTime());
            } else {
                System.out.println("Booking not possible");
                return false;
            }

        } catch (OptimisticLockingFailureException | IndexOutOfBoundsException e) {
            System.out.println("Booking failed");
            e.printStackTrace();
            return false;
        }
        return true;
    }

    public List<Reservations> browseBookedClasses(String username) {
        Optional<MongoUser> optUser = userRepository.findByUsername(username);
        if (optUser.isEmpty()) {
            System.out.println("Error: user not found.");
            return null;
        }
        MongoUser user = optUser.get();
        return user.getReservations();
    }

    public Page<Reservations> browseBookedClassesPageable(String username, int page, int size) {
        Optional<MongoUser> optUser = userRepository.findByUsername(username);
        if (optUser.isEmpty()) {
            System.out.println("Error: user not found.");
            return null;
        }
        MongoUser user = optUser.get();

        List<Reservations> userReservations = user.getReservations();
        Page<Reservations> reservations = new PageImpl<Reservations>(userReservations, PageRequest.of(page, size), userReservations.size());
        return reservations;
    }

    public List<Course> browseUserCourses(String username) {
        Optional<MongoUser> optUser = userRepository.findByUsername(username);
        if (optUser.isEmpty()) {
            System.out.println("Error: user not found.");
            return null;
        }
        MongoUser user = optUser.get();
        return user.getCourses();
    }

    public Page<Course> browseUserCoursesPageable(String username, int page, int size) {
        Optional<MongoUser> optUser = userRepository.findByUsername(username);
        if (optUser.isEmpty()) {
            System.out.println("Error: user not found.");
            return null;
        }
        MongoUser user = optUser.get();

        List<Course> userCourses = user.getCourses();
        Page<Course> courses = new PageImpl<Course>(userCourses, PageRequest.of(page, size), userCourses.size());
        return courses;
    }

    public List<Course> browseAllCoursesPageable(int page, int size) {
        Pageable paging = PageRequest.of(page, size);
        Page<Course> courses = courseRepository.findAll(paging);
        System.out.println("There are " + courses.getTotalElements() + " active courses");
        return courses.getContent();
    }

    public List<Course> browseAllCourses() {
        return courseRepository.findAll();
    }

    public boolean removeBooking(String reservationsId, String username) {
        try {
            Optional<MongoUser> optUser = userRepository.findByUsername(username);
            if (optUser.isEmpty()) {
                System.out.println("Error: user not found.");
                return false;
            }
            MongoUser user = optUser.get();

            Optional<Reservations> optReservation = reservationsRepository.findById(reservationsId);
            if (optReservation.isEmpty()) {
                System.out.println("Error: Reservations document not found.");
                return false;
            }
            Reservations reservation = optReservation.get();
            if (reservation.getActualClassTime().isBefore(LocalDateTime.now().plusMinutes(15))) {
                System.out.println("Too late: it is not possible to delete reservation at this class anymore");
                return false;
            }
            if (reservation.removeBooking(user)) {
                reservationsRepository.save(reservation);
                System.out.println("booking removed for the day " + reservation.getClassDate() +
                        ", at " + reservation.getStartTime());
                return true;
            }
        } catch (OptimisticLockingFailureException | NullPointerException | ClassCastException e) {
            e.printStackTrace();
            return false;
        }
        System.out.println("Booking deletion failed");
        return false;
    }

    public boolean leaveCourse(String courseId, String username) {
        try {
            Optional<MongoUser> optUser = userRepository.findByUsername(username);
            if (optUser.isEmpty()) {
                System.out.println("Error: user not found.");
                return false;
            }
            MongoUser client = optUser.get();

            Optional<Course> optCourse = courseRepository.findById(courseId);
            if (optCourse.isEmpty()) {
                System.out.println("Error: course not found");
                return false;
            }
            Course course = optCourse.get();

            List<Reservations> clientReservations = client.getReservations();
            for (Reservations r: clientReservations) {
                r.removeBooking(client);
                reservationsRepository.save(r);
            }
            if (client.removeCourse(course)) {
                userRepository.save(client);
                System.out.println("Subscription from course: " + course.getCourseName() +
                        " removed successfully");
                return true;
            }
        } catch (OptimisticLockingFailureException | NullPointerException | ClassCastException e) {
            e.printStackTrace();
            return false;
        }
        System.out.println("leaveCourse failed");
        return false;
    }

    // only the trainer of the course its self can do this
    public boolean deleteCourse(String courseId) {
        try {
            Optional<Course> optCourse = courseRepository.findById(courseId);
            if (optCourse.isEmpty()) {
                System.out.println("Error: course not found");
                return false;
            }
            Course course = optCourse.get();
            List<MongoUser> users = course.getEnrolledClients();
            // delete DocumentReferences in MongoUser
            for (MongoUser u: users) {
                u.removeCourse(course);
                userRepository.save(u);
            }
            reservationsRepository.deleteByCourse(course.getId());
            courseRepository.deleteById(courseId);
        } catch (OptimisticLockingFailureException | NullPointerException | ClassCastException e) {
            e.printStackTrace();
            System.err.println("Course deletion failed");
            return false;
        }
        return true;
    }

    public boolean removeClassTime(String courseId, DayOfWeek dayOfWeek, LocalTime startTime) {
        try {
            if (dayOfWeek.equals(LocalDate.now().getDayOfWeek()) &&
                    (startTime.isAfter(LocalTime.now()) &&
                            startTime.isBefore(LocalTime.now().plusMinutes(30)))) {
                System.out.println("Error: too late to remove this class, try later");
                return false;
            }

            Optional<Course> optCourse = courseRepository.findById(courseId);
            if (optCourse.isEmpty()) {
                System.out.println("Error: course not found");
                return false;
            }
            Course course = optCourse.get();
            ClassTime classToRemove = new ClassTime(dayOfWeek, startTime);
            if (course.removeClass(classToRemove)) {
                System.out.println("classTime removed successfully");
                courseRepository.save((course));
                List<Reservations> reservations = reservationsRepository.findByCourseDayTime(
                        new ObjectId(courseId), dayOfWeek, startTime.toString());
                reservationsRepository.deleteByCourseDayTime(course.getId(), dayOfWeek, startTime.toString());
            }
        } catch (OptimisticLockingFailureException | NullPointerException | ClassCastException e) {
            e.printStackTrace();
            System.err.println("Course deletion failed");
            return false;
        }
        return true;
    }

    /** a classTime modification is possible only if the class (old or new) does not start for an hour from now
     * */
    public boolean editCourseClassTime(String courseId, DayOfWeek oldDay, LocalTime oldStartTime,
                                       DayOfWeek newDay, LocalTime newStartTime, LocalTime newEndTime) {

        try {
            // todo: check if the logged user is the trainer of this course

            if (oldDay.equals(LocalDate.now().getDayOfWeek()) &&
                    ((oldStartTime.isAfter(LocalTime.now()) &&
                            oldStartTime.isBefore(LocalTime.now().plusHours(1))) ||
                            newStartTime.isBefore(LocalTime.now().plusHours(1)))) {
                System.out.println("Error: too late to modify this class, try later for the next week schedule");
                return false;
            }

            Optional<Course> optCourse = courseRepository.findById(courseId);
            if (optCourse.isEmpty()) {
                System.out.println("Error: course not found");
                return false;
            }
            Course course = optCourse.get();

            ClassTime oldClass = new ClassTime(oldDay, oldStartTime);
            if (!course.removeClass(oldClass)) {
                System.out.println("class to modify does not exists");
                return false;
            }
            // old classTime removed from list

            ClassTime newClass = new ClassTime(newDay, newStartTime, newEndTime);
            if (course.classScheduled(newClass)) {
                System.out.println("classTime modification not possible: class already scheduled at the new requested time");
                return false;
            }
            // class removed -> add the new one
            if (!course.addNewClass(newClass)) {
                System.err.println("Error: classTime modification failed");
                return false;
            }
            // new classTime added
            // get all related reservations to modify the scheduled class time
            List<Reservations> reservations = reservationsRepository.findByCourseDayTime(
                    course.getId(), oldDay, oldStartTime.toString());

            LocalDateTime newActualTime = getDatetimeFromDayAndTime(newDay, newStartTime);
            List<MongoUser> bookedUsers = new ArrayList<>();
            for (Reservations r: reservations) {
                r.setDayOfWeek(newDay);
                r.setStartTime(newStartTime.toString());
                r.setEndTime(newEndTime.toString());
                r.setActualClassTime(newActualTime);
                bookedUsers.addAll(r.getBookedUsers());
                reservationsRepository.save(r);
            }
            course = courseRepository.save(course);

            for (MongoUser u: bookedUsers) {
                /* todo: chiamare la rispettiva funzione java-erlang per le notifiche di modifica orario ai nodi
                 * -> ErlangNode/ErlangController
                 *
                 * */
            }
        } catch (OptimisticLockingFailureException | NullPointerException | ClassCastException e) {
            e.printStackTrace();
            System.err.println("editClassTime failed");
            return false;
        }
        return true;
    }

}