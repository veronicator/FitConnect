package it.unipi.dsmt.FitConnect.erlang;

import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;

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
                    System.out.println("Operation: " + response[0]);
                    String result = "";
                    for (int i = 1; i < response.length; i++) {
                        result = result + "Result n."+ i + ": " + response[i] + "; ";
                    }
                    System.out.println(result);
                }
            }
        } catch (OtpErlangExit | OtpErlangDecodeException otpErlangExit) {
            otpErlangExit.printStackTrace();
        }
    }
}
