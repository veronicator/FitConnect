package it.unipi.dsmt.fitconnect.controller;


import it.unipi.dsmt.fitconnect.entities.WSMessage;
import it.unipi.dsmt.fitconnect.services.WSService;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.HttpSession;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.*;

@RestController
public class WSController {

    @Autowired
    private WSService service;

//    @PostMapping("/send-message")
//    public void sendMessage(@RequestBody final WSMessage message) {
//        service.notifyFrontend(message.getMessageContent());
//    }

    /** l'id è l'username dell'utente della sessione */
    @PostMapping("/send-private-message/{id}")
    public ResponseEntity<String> sendPrivateMessage(@PathVariable final String id,
                                             @RequestBody final WSMessage message) {
        service.notifyUser(id, message.getMessageContent());
//        return ResponseEntity.ok().build();
        return ResponseEntity.ok("post mapping wsController ok");
    }

//    @GetMapping("/active-sessions")
//    public List<String> getActiveSessions(HttpServletRequest request, HttpServletResponse response) {
//        List<String> activeSessions = new ArrayList<>();
//
//        // Ottieni l'elenco delle sessioni attive dalla richiesta HTTP
//        for (HttpSession session : Collections.list(request.getSession(false).getServletContext().getSessionTimeout())) {
//            activeSessions.add(session.getId());
//        }
//
//        System.out.println(activeSessions);
//
//        return activeSessions;
//    }
}
