package it.unipi.dsmt.FitConnect.erlang;

public class ErlangNodeSender extends Thread{
    //private final ErlangNode clientNode;

    public ErlangNodeSender(ErlangNode clientNode) {
        //this.clientNode = clientNode;
    }

    @Override
    public void run() {
        try{
            //System.out.println("Thread Sender started");
            // IT WILL BE USED LATER TO WAIT FOR USER INPUT
        } catch (Exception e) {
            e.printStackTrace();
            System.err.println("Something failed.");
        }
    }
}