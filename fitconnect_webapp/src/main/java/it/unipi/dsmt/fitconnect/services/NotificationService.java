package it.unipi.dsmt.fitconnect.services;

import it.unipi.dsmt.fitconnect.entities.ResponseMessage;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Service;

@Service
public class NotificationService {
    private final SimpMessagingTemplate messagingTemplate;

    @Autowired
    public NotificationService(SimpMessagingTemplate messagingTemplate) {
        this.messagingTemplate = messagingTemplate;
    }

    /** method to send a message exploiting websocket
     * @param userId username (used as id) of the user to which send the message */
    public void sendPrivateNotification(final String userId) {
        ResponseMessage message = new ResponseMessage("Private Notification");

        messagingTemplate.convertAndSendToUser(userId,"/topic/private-notifications", message);

        System.out.println("DEBUG: NotificationService - sendPrivateNotification");
    }
}