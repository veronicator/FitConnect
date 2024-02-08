package it.unipi.dsmt.fitconnect.repositories.mongo;

import it.unipi.dsmt.fitconnect.entities.Message;
import org.springframework.data.mongodb.repository.MongoRepository;

public interface MessageRepositories extends MongoRepository<Message, String> {
}
