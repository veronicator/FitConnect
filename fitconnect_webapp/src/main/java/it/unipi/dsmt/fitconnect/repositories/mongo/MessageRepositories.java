package it.unipi.dsmt.fitconnect.repositories.mongo;

import it.unipi.dsmt.fitconnect.entities.Message;
import org.bson.types.ObjectId;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.data.mongodb.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface MessageRepositories extends MongoRepository<Message, String> {

    @Query(value="{'course': ?0, 'sender': { $regex: ?1, $options: 'i'} }", sort = "{'sendTime': 1}")
    List<Message> findByCourseAndSender(ObjectId courseId, String sendUsername);

    @Query(value="{'course': ?0, 'sender': { $regex: ?1, $options: 'i'} }", sort = "{'sendTime': 1}")
    Page<Message> findByCourseAndSender(ObjectId courseId, String sendUsername, Pageable pageable);

    @Query(value="{'course': ?0 }", sort = "{'sendTime': 1}")
    List<Message> findByCourse(ObjectId course);

    @Query(value="{'course': ?0 }", sort = "{'sendTime': 1}")
    List<Message> findByCourse(String course);

    @Query(value="{'course': ?0 }", sort = "{'sendTime': 1}")
    Page<Message> findByCourse(ObjectId course, Pageable pageable);
}
