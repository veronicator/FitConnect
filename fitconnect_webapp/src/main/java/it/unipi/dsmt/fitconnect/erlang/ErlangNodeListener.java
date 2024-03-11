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
                            //System.out.println(nodeMessageService.postCourseNotification(courseNotification, clientNode.getNodeName()));
                            System.out.println(courseNotification);
                        }
                        case "userJoined", "userExited" -> {
                            UserNotification userNotification = new UserNotification(response[0], response[1], response[2]);
                            String res = nodeMessageService.postUserNotification(userNotification, clientNode.getNodeName());
                            System.out.println(userNotification);
                            System.out.println(res);
                        }
                        case "message" -> {
                            Message message = new Message(response[1], response[2], response[3], LocalDateTime.now());
                            System.out.println(message);
                        }
                        default -> {
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
