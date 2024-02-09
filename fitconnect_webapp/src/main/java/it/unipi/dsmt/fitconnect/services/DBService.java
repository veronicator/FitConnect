package it.unipi.dsmt.fitconnect.services;

import it.unipi.dsmt.fitconnect.entities.*;
import it.unipi.dsmt.fitconnect.enums.UserRole;
import it.unipi.dsmt.fitconnect.repositories.mongo.*;
import lombok.Getter;
import org.bson.types.ObjectId;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.OptimisticLockingFailureException;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.temporal.TemporalAdjusters;
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

    public boolean addCourse(String courseName, String trainerUsername, Integer maxReservablePlaces) {
        try {
            Optional<MongoUser> optUser = userRepository.findByUsername(trainerUsername);
            if (optUser.isEmpty()) {
                System.out.println("addCourse failed");
                return false;
            }
            MongoUser user = optUser.get();
            // controllo da spostare nel PageController?
            if ((user.getRole().compareTo(UserRole.trainer) != 0)) {
                System.out.println("addCourse failed: logged user does not have permissions");
                return false;
            }
            if (courseRepository.existsByCourseNameAndTrainer(courseName, trainerUsername)) {
                System.out.println("same course with same trainer already exists");
                return false;
            }

            Course newCourse = new Course(courseName, user.getCompleteName(), user.getUsername(), maxReservablePlaces);
            newCourse = courseRepository.insert(newCourse);
            System.out.println("added new " + newCourse);
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
        // check if the logged user is the trainer of the referred course -> nel Controller?
        try {
            Optional<Course> optCourse = courseRepository.findById(courseId);
            if (optCourse.isEmpty()) {
                System.out.println("addClassTime failed: course not found");
                return false;
            }
            Course course = optCourse.get();
            ClassTime newClassTime = new ClassTime(dayOfWeek, startTime, endTime);
            if (course.classAlreadyScheduled(newClassTime)) {
                System.out.println("class already scheduled at the requested time");
                return false;
            }
            if (course.addNewClass(newClassTime)) {
                LocalDateTime actualTime = getDatetimeFromDayAndTime(dayOfWeek, startTime);
                courseRepository.save(course);
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
//                user.addReservation(reservations);
                System.out.println("class booking made for the day " + reservations.getClassDate());
            } else {
                System.out.println("Booking not possible: no available places");
                return false;
            }

        } catch (OptimisticLockingFailureException | IndexOutOfBoundsException e) {
            System.out.println("Booking failed");
            e.printStackTrace();
            return false;
        }
        return true;
    }

}
