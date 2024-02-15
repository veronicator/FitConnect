package it.unipi.dsmt.fitconnect.erlang;

import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import java.time.LocalDateTime;

public class ErlangNodeListener extends Thread{
    private final ErlangNode clientNode;
    private boolean running = false;

    // The Listener that receives the messages and notifications of the erlang server
    public ErlangNodeListener(ErlangNode clientNode) {
        this.clientNode = clientNode;
        this.running = true;
    }

    @Override
    public void run() {
        try {
            //System.out.println("Thread Listener started");
            while(running){
                String[] response = this.clientNode.receive();
                if(response != null){
                    switch (response[0]) {
                        case "expired":
                        case "edited":
                            CourseNotification courseNotification = new CourseNotification(response[0], response[1], response[2]);
                            System.out.println(courseNotification.toString());
                            break;
                        case "userJoined":
                        case "userExited":
                            UserNotification userNotification = new UserNotification(response[0], response[1], response[2]);
                            System.out.println(userNotification.toString());
                            break;
                        case "message":
                            Message message = new Message(response[1], response[2], response[3], LocalDateTime.now());
                            System.out.println(message.toString());
                            break;
                        default:
                            System.out.println("Operation: " + response[0]);
                            String result = "";
                            for (int i = 1; i < response.length; i++) {
                                result = result + "Result n."+ i + ": " + response[i] + "; ";
                            }
                            System.out.println(result);
                            break;
                    }
                }
            }
        } catch (OtpErlangExit | OtpErlangDecodeException otpErlangExit) {
            otpErlangExit.printStackTrace();
        }
    }
}
