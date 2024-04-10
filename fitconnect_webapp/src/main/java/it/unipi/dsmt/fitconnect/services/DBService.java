package it.unipi.dsmt.fitconnect.services;

import it.unipi.dsmt.fitconnect.entities.*;
import it.unipi.dsmt.fitconnect.enums.CourseType;
import it.unipi.dsmt.fitconnect.enums.UserRole;
import it.unipi.dsmt.fitconnect.repositories.mongo.*;
import org.bson.types.ObjectId;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.OptimisticLockingFailureException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.*;
import java.time.temporal.TemporalAdjusters;
import java.util.*;

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


    /** method for removing old reservations documents from db and creating new ones after 2 weeks
     * (reservations documents for the next week are already present in the db,
     * because created in the "addClassTime" method)
     * */
    //    @Scheduled(cron = "@midnight")
    @Scheduled(cron = "0 */1 * * * *")  // scheduled every minute
    public void updateReservationsCollection() {
        try {
            for (Reservations r : reservationsRepository.findPastReservations(LocalDateTime.now())) {
                reservationsRepository.insert(
                        new Reservations(r.getCourse(), r.getActualClassTime().plusDays(14),
                                r.getDayOfWeek(), r.getStartTime(), r.getEndTime(), r.getMaxPlaces()));
            }
            reservationsRepository.deletePastReservations(LocalDateTime.now());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /** method for obtaining the actual classTime (a LocalDateTime timestamp) for the next week,
     * starting from the day of week of the local time specified
     * @param dayOfWeek day of the week
     * @param atTime start time of the class
     * @return a LocalDateTime
     * */
    private LocalDateTime getDatetimeFromDayAndTime(DayOfWeek dayOfWeek, LocalTime atTime) {
        LocalDate nextWeekClass = LocalDate.now().with(TemporalAdjusters.next(dayOfWeek));
        return nextWeekClass.atTime(atTime);
    }

    /** method for inserting a new course
     * @param courseName name/type of the course (enum)
     * @param trainerUsername username of the trainer holding the course
     * @param maxReservablePlaces max number of places available for the course
     * @return a string indicated the id of the new course created
     * */
    @Transactional
    public String addNewCourse(CourseType courseName, String trainerUsername, Integer maxReservablePlaces) {
        try {
            Optional<MongoUser> optUser = userRepository.findByUsername("^" + trainerUsername + "$");
            if (optUser.isEmpty()) {
                System.out.println("addCourse failed: user not found");
                return null;
            }
            MongoUser trainer = optUser.get();
            if ((trainer.getRole().compareTo(UserRole.trainer) != 0)) {
                System.out.println("addCourse failed: logged user does not have permissions");
                return null;
            }

            String course = "^" + courseName.toString() + "$";
            String trainerName = "^" + trainer.getUsername() + "$";
            if (courseRepository.existsByCourseNameAndTrainer(course, trainerName)) {
                System.out.println("same course with same trainer already exists");
                return null;
            }

            Course newCourse = new Course(courseName.toString(), trainer.getCompleteName(), trainer.getUsername(), maxReservablePlaces);
            newCourse = courseRepository.insert(newCourse);
            trainer.addCourse(newCourse);
            userRepository.save(trainer);
            System.out.println("new course added: " + newCourse.getCourseName());
            return newCourse.getId().toString();
        } catch (Exception e) {
            System.out.println("addCourse failed");
            e.printStackTrace();
            return null;
        }
    }

    /** method for adding a new classTime for a course schedule
     * moreover, also Reservations mongo documents in the db are created for the next 2 weeks
     * e.g. if the specified dayOfWeek is Monday, the document for the next Monday and
     * the following Monday are added
     * In this way, class booking are allowed for the next 2 weeks
     * @param courseId id of the course in which you want to add the new class time
     * @param dayOfWeek day of the week you want to hold the course lesson
     * @param startTime time at which you want to start the lesson
     * @param endTime end time of the lesson
     * @return the Course object of the course at which the class time was added */
    @Transactional
    public Course addClassTime(String courseId, DayOfWeek dayOfWeek, LocalTime startTime, LocalTime endTime) {

        try {
            if (startTime.isAfter(endTime)) {
                System.out.println("Error: startTime can not be after endTime");
                return null;
            }
            Optional<Course> optCourse = courseRepository.findById(courseId);
            if (optCourse.isEmpty()) {
                System.out.println("addClassTime failed: course not found");
                return null;
            }
            Course course = optCourse.get();
            ClassTime newClassTime = new ClassTime(dayOfWeek, startTime, endTime);
            if (course.classScheduled(newClassTime)) {
                System.out.println("class already scheduled at the requested time");
                return null;
            }
            if (course.addNewClass(newClassTime)) {
                LocalDateTime actualTime = getDatetimeFromDayAndTime(dayOfWeek, startTime);
                course = courseRepository.save(course);
                // creating next week reservations document
                reservationsRepository.insert(
                        new Reservations(
                                course, actualTime, dayOfWeek, startTime, endTime,
                                course.getMaxReservablePlaces()
                        )
                );
                // creating next 2 week reservations mongo document
                reservationsRepository.insert(
                        new Reservations(
                                course, actualTime.plusDays(7), dayOfWeek, startTime, endTime,
                                course.getMaxReservablePlaces()
                        )
                );
            }
            return course;
        } catch (OptimisticLockingFailureException e) {
            e.printStackTrace();
            return null;
        }
    }

    /** method for allowing a client joining a course
     * @param courseId id of the course the client want to join
     * @param username username of the client who wants to join the course
     * @return true in case of success, false for failure */
    public boolean joinCourse(String courseId, String username) {
        try {
            Optional<MongoUser> optUser = userRepository.findByUsername("^" + username + "$");
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

    /** method for booking a lesson
     * @param username username of the client who wants to book the lesson
     * @param courseId id of the course for which the client wants to book the lesson
     * @param dayOfWeek day of the week of the lesson the client wants to book
     * @param startTime starting time of the lesson to book
     * @return the Reservations object for which the booking was made*/
    public Reservations bookClass(String username, String courseId, DayOfWeek dayOfWeek, String startTime) {
        try {
            Optional<MongoUser> optUser = userRepository.findByUsername("^" + username + "$");
            if (optUser.isEmpty()) {
                System.out.println("Booking failed: user not found.");
                return null;
            }
            MongoUser user = optUser.get();

            List<Reservations> availableClasses = reservationsRepository.findByCourseDayTime(
                    new ObjectId(courseId), dayOfWeek, startTime);
            if (availableClasses.isEmpty()) {
                System.out.println("Booking failed: no available courses for the day-time selected found");
                return null;
            }

            Reservations reservations = availableClasses.get(0);
            if (reservations.getActualClassTime().isBefore(LocalDateTime.now())) {
                if (availableClasses.size() > 1)
                    reservations = availableClasses.get(1);
                else {
                    System.out.println("Booking not possible: class already held\n" +
                            "Try to book tomorrow");
                    return null;
                }
            }

            if (reservations.isBooked(user)) {
                System.out.println("class already booked");
                return reservations;
            }
            if (reservations.addBooking(user)) {
                reservations = reservationsRepository.save(reservations);
                System.out.println("class booking made for the day " + reservations.getClassDate() +
                        ", at " + reservations.getStartTime());
                return reservations;
            } else {
                System.out.println("Booking not possible: no more places available");
                return null;
            }
        } catch (OptimisticLockingFailureException | IndexOutOfBoundsException e) {
            System.out.println("Booking failed");
            e.printStackTrace();
            return null;
        }
    }

    /** method for getting all reservations made by a single client
     * @param username client username
     * @return the list of Reservations objects */
    public List<Reservations> browseBookedClasses(String username) {
        Optional<MongoUser> optUser = userRepository.findByUsername("^" + username + "$");
        if (optUser.isEmpty()) {
            System.out.println("Error: user not found.");
            return null;
        }
        MongoUser user = optUser.get();
        return user.getReservations();
    }

    /** method for getting reservations made by a single client in a pageable manner
     * @param username client username
     * @param page page you want to get
     * @param size size of the page
     * @return Page of the list of Reservations objects*/
    public Page<Reservations> browseBookedClassesPageable(String username, int page, int size) {
        Optional<MongoUser> optUser = userRepository.findByUsername("^" + username + "$");
        if (optUser.isEmpty()) {
            System.out.println("Error: user not found.");
            return null;
        }
        MongoUser user = optUser.get();
        List<Reservations> userReservations = user.getReservations();
        return new PageImpl<>(userReservations, PageRequest.of(page, size), userReservations.size());
    }

    /** method to obtain courses of a user (join or teach), in a pageable manner
     * @param username username of the user you want to get courses
     * @param page page you want to get
     * @param size size of the page
     * @return Page of the list of Course object */
    public Page<Course> browseUserCoursesPageable(String username, int page, int size) {
        Optional<MongoUser> optUser = userRepository.findByUsername("^" + username + "$");
        if (optUser.isEmpty()) {
            System.out.println("Error: user not found.");
            return null;
        }
        MongoUser user = optUser.get();

        List<Course> userCourses = user.getCourses();
        return new PageImpl<>(userCourses, PageRequest.of(page, size), userCourses.size());
    }

    /** method to obtain all courses in a pageable manner
     * @param page page you want to get
     * @param size size of the page
     * @return List of the course */
    public List<Course> browseAllCoursesPageable(int page, int size) {
        Pageable paging = PageRequest.of(page, size);
        Page<Course> courses = courseRepository.findAll(paging);
        System.out.println("There are " + courses.getTotalElements() + " active courses");
        return courses.getContent();
    }

    /** method to obtain all the courses on the platform
     * @return List of Course objects */
    public List<Course> browseAllCourses() {
        return courseRepository.findAll();
    }

    /** method for obtaining the courses of a specified type/name
     * @param courseType type of the course (name of the correspondent enumerator)
     * @return the List of founded courses */
    public List<Course> browseCourses(String courseType) {
        String courseName = "^" + courseType + "$";
        return courseRepository.findByCourseName(courseName);
    }

    /** method for remove a lesson reservation
     * @param reservationsId id of the reservation mongo document to modify for removing the client reservation
     * @param username client username who wants to remove the reservation
     * @return the id of the updated reservation document */
    public String unbookClass(String reservationsId, String username) {
        try {
            Optional<MongoUser> optUser = userRepository.findByUsername("^" + username + "$");
            if (optUser.isEmpty()) {
                System.out.println("Error: user not found.");
                return null;
            }
            MongoUser user = optUser.get();

            Optional<Reservations> optReservation = reservationsRepository.findById(reservationsId);
            if (optReservation.isEmpty()) {
                System.out.println("Error: Reservations document not found.");
                return null;
            }
            Reservations reservation = optReservation.get();
            if (reservation.getActualClassTime().isBefore(LocalDateTime.now().plusMinutes(15))) {
                System.out.println("Too late: it is not possible to delete reservation at this class anymore");
                return null;
            }
            if (reservation.removeBooking(user)) {
                reservation = reservationsRepository.save(reservation);
                System.out.println("booking removed for the day " + reservation.getClassDate() +
                        ", at " + reservation.getStartTime());
                return reservation.getId().toString();
            }
        } catch (OptimisticLockingFailureException | NullPointerException | ClassCastException e) {
            e.printStackTrace();
            return null;
        }
        System.out.println("Booking deletion failed");
        return null;
    }

    /** method for removing the subscription of a client from a course
     * @param courseId id of the course to leave
     * @param username username of the client who wants to leave the course
     * @return true on success, false on failure */
    public boolean leaveCourse(String courseId, String username) {
        try {
            Optional<MongoUser> optUser = userRepository.findByUsername("^" + username + "$");
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

            List<Reservations> clientReservations = reservationsRepository.findByCourseAndUser(
                    course.getId(), username);
            for (Reservations r : clientReservations) {
                r.removeBooking(client);
                reservationsRepository.save(r);
            }
            if (client.removeCourse(course)) {
                userRepository.save(client);
                System.out.println("Subscription from course '" + course.getCourseName() +
                        "' removed successfully");
                return true;
            }
        } catch (OptimisticLockingFailureException | NullPointerException | ClassCastException e) {
            e.printStackTrace();
            return false;
        }
        System.out.println("leaveCourse failed");
        return false;
    }

    /** method for deleting a course, removing also all the related Reservations documents in mongodb
     * and associated reference in the users collection
     * @param courseId id of the course to remove
     * @return true on success, false on failure */
    @Transactional
    public boolean deleteCourse(String courseId) {
        try {
            Optional<Course> optCourse = courseRepository.findById(courseId);
            if (optCourse.isEmpty()) {
                System.out.println("Error: course not found");
                return false;
            }
            Course course = optCourse.get();
            List<MongoUser> users = course.getEnrolledClients();
            // removing DocumentReferences in MongoUser
            for (MongoUser u : users) {
                u.removeCourse(course);
                userRepository.save(u);
            }

            reservationsRepository.deleteByCourse(course.getId());
            courseRepository.deleteById(courseId);
            return true;
        } catch (OptimisticLockingFailureException | NullPointerException | ClassCastException e) {
            e.printStackTrace();
            System.err.println("Course deletion failed");
            return false;
        }
    }

    /** method to remove a lesson from course schedule,
     * this operation is possible only if the class (old or new) does not start for an hour from now
     * remove also all the related Reservations Mongo document
     * @param courseId id of the course of the lesson to remove
     * @param dayOfWeek day of week of the lesson to remove
     * @param startTime starting time of the lesson to remove
     * @return List of Reservations of the lesson before removing them from the db */
    @Transactional
    public List<Reservations> removeClassTime(String courseId, DayOfWeek dayOfWeek, LocalTime startTime) {
        try {
            if (dayOfWeek.equals(LocalDate.now().getDayOfWeek()) &&
                    (startTime.isAfter(LocalTime.now()) &&
                            startTime.isBefore(LocalTime.now().plusMinutes(60)))) {
                System.out.println("Error: too late to remove this class, try later for the next times");
                return null;
            }

            Optional<Course> optCourse = courseRepository.findById(courseId);
            if (optCourse.isEmpty()) {
                System.out.println("Error: course not found");
                return null;
            }
            Course course = optCourse.get();
            ClassTime classToRemove = new ClassTime(dayOfWeek, startTime);
            if (course.removeClass(classToRemove)) {
                System.out.println("classTime removed successfully");
                courseRepository.save((course));
                List<Reservations> reservations = reservationsRepository.findByCourseDayTime(
                        new ObjectId(courseId), dayOfWeek, startTime.toString());

                reservationsRepository.deleteByCourseDayTime(course.getId(), dayOfWeek, startTime.toString());
                return reservations;
            }
        } catch (OptimisticLockingFailureException | NullPointerException | ClassCastException e) {
            e.printStackTrace();
            System.err.println("Course deletion failed");
            return null;
        }
        return null;
    }

    /** method to edit a lesson time
     * a classTime modification is possible only if the class (old or new) does not start for an hour from now
     * @param courseId course to edit the lesson time
     * @param oldDay old day of week of the lesson
     * @param oldStartTime old start time of the lesson
     * @param newDay new day of the week for the lesson
     * @param newStartTime new start time of the lesson
     * @param newEndTime new end time of the lesson
     * @return the list of Reservations after the update */
    @Transactional
    public List<Reservations> editCourseClassTime(String courseId, DayOfWeek oldDay, LocalTime oldStartTime,
                                                  DayOfWeek newDay, LocalTime newStartTime, LocalTime newEndTime) {

        try {
            if ((oldDay.equals(LocalDate.now().getDayOfWeek()) &&
                    (oldStartTime.isAfter(LocalTime.now()) &&
                            oldStartTime.isBefore(LocalTime.now().plusHours(1)))
            ) || (newDay.equals(LocalDate.now().getDayOfWeek()) &&
                    (newStartTime.isAfter(LocalTime.now()) &&
                    newStartTime.isBefore(LocalTime.now().plusHours(1)))
                )
            ) {
                System.out.println("Error: too late to modify this class, try later for the next week schedule");
                return null;
            }

            Optional<Course> optCourse = courseRepository.findById(courseId);
            if (optCourse.isEmpty()) {
                System.out.println("Error: course not found");
                return null;
            }
            Course course = optCourse.get();

            ClassTime oldClass = new ClassTime(oldDay, oldStartTime);
            if (!course.removeClass(oldClass)) {
                System.out.println("class to modify does not exists");
                return null;
            }
            // old classTime removed from list

            ClassTime newClass = new ClassTime(newDay, newStartTime, newEndTime);
            if (course.classScheduled(newClass)) {
                System.out.println("classTime modification not possible: class already scheduled at the new requested time");
                return null;
            }
            // class removed -> add the new one
            if (!course.addNewClass(newClass)) {
                System.err.println("Error: classTime modification failed");
                return null;
            }
            // new classTime added
            // get all related reservations to modify the scheduled class time
            List<Reservations> reservations = reservationsRepository.findByCourseDayTime(
                    course.getId(), oldDay, oldStartTime.toString());

            LocalDateTime newActualTime = getDatetimeFromDayAndTime(newDay, newStartTime);
            for (Reservations r : reservations) {
                r.setDayOfWeek(newDay);
                r.setStartTime(newStartTime.toString());
                r.setEndTime(newEndTime.toString());
                r.setActualClassTime(newActualTime);
                reservationsRepository.save(r);
            }
            courseRepository.save(course);
            return reservationsRepository.findByCourseDayTime(course.getId(), newDay, newStartTime.toString());
        } catch (OptimisticLockingFailureException | NullPointerException | ClassCastException e) {
            e.printStackTrace();
            System.err.println("editClassTime failed");
            return null;
        }
    }

    /** method to obtain the Course object
     * @param courseId id of the course to get
     * @return the course object or null if not found */
    public Course getCourse(String courseId) {
        Optional<Course> optCourse = courseRepository.findById(courseId);
        return optCourse.orElse(null);
    }

    public String getCourseName(String courseId) {
        Optional<Course> optCourse = courseRepository.findById(courseId);
        return optCourse.map(Course::getCourseName).orElse(null);
    }

    /** method to obtain the course object, specifying the course name/type and the trainer
     * @param courseName name of the course to get (value of the CourseType enum)
     * @param trainerUsername username of the trainer who holds the course
     * @return the Course object found, null if no course is found */
    public Course getByCourseAndTrainer(String courseName, String trainerUsername) {
        String course = "^" + courseName + "$";
        String trainer = "^" + trainerUsername + "$";
        Optional<Course> optCourse = courseRepository.findByCourseNameAndTrainerUsername(course, trainer);
//        return courseRepository.findByCourseNameAndTrainerUsername(courseName, trainerUsername);
        return optCourse.orElse(null);
    }

    public List<Message> getMessages(String courseId) {
        return messageRepositories.findByCourse(new ObjectId(courseId));
    }

    /**
     * Function for creating user object to store into Mongo database
     * @param user: user data
     * @return true|false
     */
    public boolean createMongoUser(final MongoUser user) {
        // insert user into mongo db
        try {
            userRepository.save(user);
            System.out.println("User: " + user.getUsername() + " registered in mongodb");
            return true;
        } catch (Exception e) {
            System.out.println("error in mongodb registration: " + e.getMessage());
            return false;
        }
    }

    /** method to get the MongoUser object of a specific user
     * @param username username of the user to find
     * @return the MongoUser object found, null if no user is found */
    public MongoUser getUser(String username) {
        Optional<MongoUser> optUser = userRepository.findByUsername("^" + username + "$");
        return optUser.orElse(null);
    }

    /** method to check if a user exists using its username
     * @param username username of the user
     * @return true|false */
    public boolean existsByUsername(String username) {
        return userRepository.existsByUsername("/^" + username + "$");
    }

    /** method to get a Reservation object
     * @param reservationsId id of the reservations document to get
     * @return the Reservation object found, null if no document found*/
    public Reservations getReservations(String reservationsId) {
        Optional<Reservations> optReservation = reservationsRepository.findById(reservationsId);
        return optReservation.orElse(null);
    }

    /** method to obtain all Reservations related to a specific course
     * @param courseId id of the course you want the associated reservations
     * @return list of the Reservations found */
    public List<Reservations> getReservationsByCourse(String courseId) {
        return reservationsRepository.findByCourse(new ObjectId(courseId));
    }

    /** method to obtain all Reservations related to a specific course for a specific client
     * @param courseId id of the course you want the associated reservations
     * @param username username of the client you want the reservations
     * @return list of the Reservations found*/
    public List<Reservations> getReservationsByCourseAndUser(String courseId, String username) {
        return reservationsRepository.findByCourseAndUser(new ObjectId(courseId), username);
    }

    /**
     * method to retrieve message with a pageable structure
     * @param username
     * @param room course ID
     * @param pageNumber
     * @return
     */
    public List<Message> getCourseMessages(String username, String room, int pageNumber) {
        List<Course> courses = getUser(username).getCourses();
        Course targetCourse = getCourse(room);

        if (targetCourse == null) {
            return null;
        }

        if (!courses.contains(targetCourse)) {
            System.out.println("user is not in this course");
            return null;
        }

        int pageSize = 3; // dimensione della pagina

        List<Message> messages = messageRepositories
                .findByCourse(room, PageRequest.of(pageNumber, pageSize))
                .getContent();

        List<Message> mutableMessages = new ArrayList<>(messages);

        return mutableMessages;
    }

    public void saveMessage(Message message) {
        messageRepositories.save(message);
    }


}
