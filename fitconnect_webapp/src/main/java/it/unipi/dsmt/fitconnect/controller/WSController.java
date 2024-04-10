package it.unipi.dsmt.fitconnect.controller;


import it.unipi.dsmt.fitconnect.entities.WSMessage;
import it.unipi.dsmt.fitconnect.services.WSService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class WSController {

    @Autowired
    private WSService service;

    /**
     * @param id session username
     * @param message
     * @return
     */
    @PostMapping("/send-private-message/{id}")
    public ResponseEntity<String> sendPrivateMessage(@PathVariable final String id,
                                             @RequestBody final WSMessage message) {
        service.notifyUser(id, message.getMessageContent());
        return ResponseEntity.ok("post mapping WSController ok");
    }
}
