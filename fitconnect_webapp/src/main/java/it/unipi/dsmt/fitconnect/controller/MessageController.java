package it.unipi.dsmt.fitconnect.controller;

import it.unipi.dsmt.fitconnect.entities.ChatMessage;
import it.unipi.dsmt.fitconnect.entities.Message;
import it.unipi.dsmt.fitconnect.entities.ResponseMessage;
import it.unipi.dsmt.fitconnect.entities.WSMessage;
import it.unipi.dsmt.fitconnect.services.DBService;
import it.unipi.dsmt.fitconnect.services.NotificationService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.messaging.handler.annotation.DestinationVariable;
import org.springframework.messaging.handler.annotation.MessageMapping;
import org.springframework.messaging.handler.annotation.SendTo;
import org.springframework.messaging.simp.annotation.SendToUser;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.util.HtmlUtils;

import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

@Controller
public class MessageController {
    @Autowired
    private NotificationService notificationService;
    @Autowired
    private DBService dbService;

    // Map to store different chat rooms
    private Map<String, String> chatRooms = new HashMap<>();

//    @MessageMapping("/message")
//    @SendTo("/topic/messages")
//    public ResponseMessage getMessage(final WSMessage message) throws InterruptedException {
//        Thread.sleep(1000);
//        notificationService.sendGlobalNotification();
//        return new ResponseMessage(HtmlUtils.htmlEscape(message.getMessageContent()));
//    }

    @MessageMapping("/private-message")
    @SendToUser("/topic/private-messages")
    public ResponseMessage getPrivateNotification(final WSMessage message,
                                             final String username) throws InterruptedException {
        Thread.sleep(1000);
        notificationService.sendPrivateNotification(username);

        System.out.println("DEBUG: MessageController - getPrivateNotification function");

        return new ResponseMessage(HtmlUtils.htmlEscape(
                "Sending private message to user " + username + ": "
                        + message.getMessageContent())
        );
    }

    @MessageMapping("/{room}/chat")  //end point a cui i client devono mandare il messaggio
    @SendTo("/topic/{room}/chat")   //tutti i client iscritti a questo canale riceveranno il messaggio di questo metodo
    public Message sendMessage(@DestinationVariable String room, ChatMessage message) throws Exception {
        Message outMessage = new Message();
        outMessage.setSender(message.getSender());
        outMessage.setText(message.getText());
        outMessage.setSendTime(LocalDateTime.now());    //todo va bene?
        outMessage.setCourse(room);

        dbService.saveMessage(outMessage);

        return outMessage;
    }

    @PostMapping("/createRoom")
    public ResponseEntity<String> createRoom(@RequestParam String roomName) {       // roomName = courseId
        if (!chatRooms.containsKey(roomName)) {
            chatRooms.put(roomName, roomName);
            return ResponseEntity.ok("Room created successfully");
        } else {
            return ResponseEntity.badRequest().body("Room already exists");
        }
    }
}
