package it.unipi.dsmt.fitconnect.config;

import jakarta.servlet.http.HttpSession;
import jakarta.websocket.EndpointConfig;
import jakarta.websocket.OnOpen;
import jakarta.websocket.Session;
import jakarta.websocket.server.ServerEndpoint;

@ServerEndpoint(value = "/profile", configurator = GetHttpSessionConfigurator.class)
public class GetHttpSessionSocket {
    private Session wsSession;
    private HttpSession httpSession;

    @OnOpen
    public void open(Session session, EndpointConfig config) {
        this.wsSession = session;
        this.httpSession = (HttpSession) config.getUserProperties().get(HttpSession.class.getName());
    }
}
