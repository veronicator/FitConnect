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

    @MessageMapping("/private-message")
    @SendToUser("/topic/private-messages")
    public ResponseMessage getPrivateNotification(final WSMessage message,
                                             final String username) throws InterruptedException {
        Thread.sleep(1000);
        notificationService.sendPrivateNotification(username);

        return new ResponseMessage(HtmlUtils.htmlEscape(
                "Sending private message to user " + username + ": "
                        + message.getMessageContent())
        );
    }

    @MessageMapping("/{room}/chat")
    @SendTo("/topic/{room}/chat")   //all user enrolled with this course will receive messages in this channel
    public Message sendMessage(@DestinationVariable String room, ChatMessage message) throws Exception {
        Message outMessage = new Message();
        outMessage.setSender(message.getSender());
        outMessage.setText(message.getText());
        outMessage.setSendTime(LocalDateTime.now());
        outMessage.setCourse(room);

        dbService.saveMessage(outMessage);

        return outMessage;
    }
}
