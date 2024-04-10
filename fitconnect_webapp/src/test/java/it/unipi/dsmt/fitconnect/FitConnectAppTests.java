package it.unipi.dsmt.fitconnect;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.time.DayOfWeek;
import java.time.LocalTime;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import it.unipi.dsmt.fitconnect.entities.Course;
import it.unipi.dsmt.fitconnect.entities.MongoUser;
import it.unipi.dsmt.fitconnect.entities.Reservations;
import it.unipi.dsmt.fitconnect.enums.CourseType;
import it.unipi.dsmt.fitconnect.enums.UserRole;
import it.unipi.dsmt.fitconnect.repositories.mongo.MongoUserRepository;
import it.unipi.dsmt.fitconnect.services.DBService;

@SpringBootTest
public class FitConnectAppTests {
    @Autowired
    DBService dbService;
    @Autowired
    MongoUserRepository mongoRepository;

    @Test
    void concurrentBooking() throws InterruptedException {
        MongoUser trainerTest = dbService.getUser("test");
        assertNotNull(trainerTest);
        System.out.println("user test found");

        String newCourseId = dbService.addNewCourse(CourseType.Ballet, trainerTest.getUsername(), 5);
        assertNotNull(newCourseId);
        System.out.println("new Course created");

        String startTime = "17:00";
        String endTime = "18:00";
        Course course = dbService.addClassTime(newCourseId, DayOfWeek.FRIDAY, LocalTime.parse(startTime), LocalTime.parse(endTime));
        assertNotNull(course);
        System.out.println("new class time added");
        
        Pageable paging = PageRequest.of(1, 2);
        List<MongoUser> clients = mongoRepository.findByRole(UserRole.client, paging).getContent();
        assertNotNull(clients);
        for (MongoUser user: clients) {
            boolean joined = dbService.joinCourse(newCourseId, user.getUsername());
            assertTrue(joined);
        }

        ExecutorService executorService = Executors.newFixedThreadPool(2);
        for (MongoUser user: clients) {
            executorService.execute(
                () -> {
                    System.out.println(user.getUsername() + " is trying to book");
                    Reservations reservation = dbService.bookClass(user.getUsername(), newCourseId, DayOfWeek.FRIDAY, startTime);
                    //assertNotNull(reservation);
                    System.out.println(user.getUsername() + " booking result: " + (reservation != null));
                }
            );
        }     
        
        executorService.shutdown();
        executorService.awaitTermination(1, TimeUnit.MINUTES);

        Course course2 = dbService.getCourse(newCourseId);
        assertEquals(course2.getVersion(), 1);
        System.out.println("Course version is 1");

        dbService.deleteCourse(newCourseId);

    }

    @Test
    void overbookingTest() throws InterruptedException {
        MongoUser trainerTest = dbService.getUser("test");
        assertNotNull(trainerTest);
        System.out.println("user test found");

        String newCourseId = dbService.addNewCourse(CourseType.Ballet, trainerTest.getUsername(), 5);
        assertNotNull(newCourseId);
        System.out.println("new Course created");

        String startTime = "17:00";
        String endTime = "18:00";
        Course course = dbService.addClassTime(newCourseId, DayOfWeek.FRIDAY, LocalTime.parse(startTime), LocalTime.parse(endTime));
        assertNotNull(course);
        System.out.println("new class time added");
        
        Pageable paging = PageRequest.of(1, 7);
        List<MongoUser> clients = mongoRepository.findByRole(UserRole.client, paging).getContent();
        assertNotNull(clients);
        for (MongoUser user: clients) {
            boolean joined = dbService.joinCourse(newCourseId, user.getUsername());
            assertTrue(joined);
            Reservations reservation = dbService.bookClass(user.getUsername(), newCourseId, DayOfWeek.FRIDAY, startTime);
            System.out.println(user.getUsername() + " booking result: " + (reservation != null));
        }
        
        Reservations reservations = dbService.getReservationsByCourse(newCourseId).get(0);
        assertEquals(reservations.getReservablePlaces(), 0);
        System.out.println("Class has " + reservations.getBookedUsers().size() + " clients booked");
        System.out.println("course max reservable places: " +  reservations.getMaxPlaces());

        dbService.deleteCourse(newCourseId);
    }

    @Test
    void concurrentJoinDelete() throws InterruptedException {
        MongoUser trainerTest = dbService.getUser("test");
        assertNotNull(trainerTest);
        System.out.println("user test found");

        String newCourseId = dbService.addNewCourse(CourseType.Ballet, trainerTest.getUsername(), 5);
        assertNotNull(newCourseId);
        System.out.println("new Course created");

        for (int i=0; i<2; i++) {
            MongoUser userClient = new MongoUser("username"+i, "firstname", "lastname",  i+"email@email.com", UserRole.client);
            boolean created = dbService.createMongoUser(userClient);
            assertTrue(created);
            System.out.println("user client " + " created");
        }


        Thread joinThread0 = new Thread(
            () -> {
                System.out.println("user client 0 is trying to join course");
                boolean ret = dbService.joinCourse(newCourseId, "username0");
                System.out.println("user client 0 join result: " + ret);
            }
        );
        Thread joinThread1 = new Thread(
            () -> {
                System.out.println("user client 1 is trying to join course");
                boolean ret = dbService.joinCourse(newCourseId, "username1");
                System.out.println("user client 1 join result: " + ret);
            }
        );

        Thread deleteThread = new Thread(
            () -> {
                System.out.println("trainerTest is trying to delete course");
                boolean ret = dbService.deleteCourse(newCourseId);
                System.out.println("trainerTest delete result: " + ret);
            }
        );

        ExecutorService executorService = Executors.newFixedThreadPool(2);
        executorService.execute(deleteThread);
        executorService.execute(joinThread0);
        executorService.execute(joinThread1);

        executorService.shutdown();
        executorService.awaitTermination(1, TimeUnit.MINUTES);

        Course course = dbService.getCourse(newCourseId);
        if (course == null) {
            System.out.println("Course successfully deleted");
        } else {
            assertEquals(course.getVersion(), 1);
            System.out.println("Course version is 1");
        }

        dbService.deleteCourse(newCourseId);
        mongoRepository.deleteByUsername("username0");
        mongoRepository.deleteByUsername("username1");
    }

}
