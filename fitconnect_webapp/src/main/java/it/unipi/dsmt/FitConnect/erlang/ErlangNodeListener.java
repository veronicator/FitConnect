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
                    if (response.length == 2){
                        System.out.println("Node: " + clientNode.getNodeName() + " ->{ Action: " + response[0] + "; Result: " + response[1] + " }");
                    } else if (response.length == 3){
                        System.out.println("Node: " + clientNode.getNodeName() + " ->{ Action: " + response[0] + "; Sender: " + response[1] + "; Message: " + response[1] +" }");
                    }
                }
            }
        } catch (OtpErlangExit | OtpErlangDecodeException otpErlangExit) {
            otpErlangExit.printStackTrace();
        }
    }
}
