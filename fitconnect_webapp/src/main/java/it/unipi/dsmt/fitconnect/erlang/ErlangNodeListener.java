package it.unipi.dsmt.fitconnect.erlang;

import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import it.unipi.dsmt.fitconnect.entities.CourseNotification;
import it.unipi.dsmt.fitconnect.entities.Message;
import it.unipi.dsmt.fitconnect.entities.UserNotification;
import it.unipi.dsmt.fitconnect.services.NodeMessageService;

import java.time.LocalDateTime;

public class ErlangNodeListener extends Thread {

    private final ErlangNode clientNode;
    private final NodeMessageService nodeMessageService;

    // The Listener that receives the messages and notifications of the erlang server
    public ErlangNodeListener(ErlangNode clientNode, NodeMessageService nodeMessageService) {
        this.clientNode = clientNode;
        this.nodeMessageService = nodeMessageService;
    }

    @Override
    public void run() {
        try {
            while(true){
                String[] response = this.clientNode.receive();
                if(response != null){
                    switch (response[0]) {
                        case "expired", "edited" -> {
                            CourseNotification courseNotification = new CourseNotification(response[0], response[1], response[2]);
                            nodeMessageService.postCourseNotification(courseNotification, clientNode.getNodeName());
                            //System.out.println(nodeMessageService.postCourseNotification(courseNotification, clientNode.getNodeName()));
                            //System.out.println(courseNotification);
                        }
                        case "userJoined", "userExited" -> {
                            UserNotification userNotification = new UserNotification(response[0], response[1], response[2]);
                            nodeMessageService.postUserNotification(userNotification, clientNode.getNodeName());
                            //System.out.println(nodeMessageService.postUserNotification(userNotification, clientNode.getNodeName()));
                            //System.out.println(userNotification);
                        }
                        case "message" -> {
                            // This was not implemented in java since it would have required too much time
                            Message message = new Message(response[1], response[2], response[3], LocalDateTime.now());
                            System.out.println(message);
                        }
                        default -> { // It is any operation that receives a confirmation
                            System.out.println("Operation: " + response[0]);
                            StringBuilder result = new StringBuilder();
                            for (int i = 1; i < response.length; i++) {
                                result.append("Result n.").append(i).append(": ").append(response[i]).append("; ");
                            }
                            System.out.println(result);
                        }
                    }
                }
            }
        } catch (OtpErlangExit | OtpErlangDecodeException otpErlangExit) {
            otpErlangExit.printStackTrace();
        }
    }
}
