package it.unipi.dsmt.fitconnect.controller;


import it.unipi.dsmt.fitconnect.entities.WSMessage;
import it.unipi.dsmt.fitconnect.services.WSService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class WSController {

    @Autowired
    private WSService service;

//    @PostMapping("/send-message")
//    public void sendMessage(@RequestBody final WSMessage message) {
//        service.notifyFrontend(message.getMessageContent());
//    }

    /** l'id è in realtà l'username dell'utente */
    @PostMapping("/send-private-message/{id}")
    public void sendPrivateMessage(@PathVariable final String id,
                                   @RequestBody final WSMessage message) {
        service.notifyUser(id, message.getMessageContent());
    }
}
