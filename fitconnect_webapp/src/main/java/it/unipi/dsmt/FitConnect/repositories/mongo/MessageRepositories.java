package it.unipi.dsmt.FitConnect.repositories.mongo;

import it.unipi.dsmt.FitConnect.entities.Message;
import org.springframework.data.mongodb.repository.MongoRepository;

public interface MessageRepositories extends MongoRepository<Message, String> {
}
