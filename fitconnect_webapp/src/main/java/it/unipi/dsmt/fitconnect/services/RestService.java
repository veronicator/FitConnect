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

    public String postNotification() {
        String url = "http://localhost:8080/send-private-message/{username}";
        // create headers
        HttpHeaders headers = new HttpHeaders();
        // set `content-type` header
        headers.setContentType(MediaType.APPLICATION_JSON);
        // set `accept` header
//        headers.setAccept(Collections.singletonList(MediaType.APPLICATION_JSON));

        WSMessage message = new WSMessage("spring post");
        HttpEntity<WSMessage> entity = new HttpEntity<>(message, headers);
//        return restTemplate.postForObject(url, entity, WSMessage.class);
        ResponseEntity<String> response = restTemplate.exchange(
                url, HttpMethod.POST, entity, String.class, "test");
        System.out.println("Risposta dal server: " + response.getBody());
        return response.getBody();
    }

    public String postNotification(String username, String messageToSend) {
        String url = "http://localhost:8080/send-private-message/{username}";
        // create headers
        HttpHeaders headers = new HttpHeaders();
        // set `content-type` header
        headers.setContentType(MediaType.APPLICATION_JSON);
        // set `accept` header
//        headers.setAccept(Collections.singletonList(MediaType.APPLICATION_JSON));

        WSMessage message = new WSMessage(messageToSend);
        HttpEntity<WSMessage> entity = new HttpEntity<>(message, headers);
//        return restTemplate.postForObject(url, entity, WSMessage.class);
        ResponseEntity<String> response = restTemplate.exchange(
                url, HttpMethod.POST, entity, String.class, username);
        System.out.println("Risposta dal server: " + response.getBody());
        return response.getBody();
    }

}
