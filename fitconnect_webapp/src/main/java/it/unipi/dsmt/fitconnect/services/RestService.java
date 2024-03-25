package it.unipi.dsmt.fitconnect.services;

import it.unipi.dsmt.fitconnect.entities.WSMessage;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.http.*;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

@Service
public class RestService {
    private final RestTemplate restTemplate;

    public RestService(RestTemplateBuilder restTemplateBuilder) {
        this.restTemplate = restTemplateBuilder.build();
    }

    /* POST methods */


    /** method to post a notification for a specific user
     * @param username username of the user to which send the notification
     * @param messageToSend text of the notification to send
     * @return a string containing the response body of the message sent */
    public String postNotification(String username, String messageToSend) {
        String url = "http://localhost:8080/send-private-message/{username}";
        // create headers
        HttpHeaders headers = new HttpHeaders();
        // set `content-type` header
        headers.setContentType(MediaType.APPLICATION_JSON);

        WSMessage message = new WSMessage(messageToSend);
        HttpEntity<WSMessage> entity = new HttpEntity<>(message, headers);
        ResponseEntity<String> response = restTemplate.exchange(
                url, HttpMethod.POST, entity, String.class, username);
        return response.getBody();
    }

}
