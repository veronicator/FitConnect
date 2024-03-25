package it.unipi.dsmt.fitconnect.erlang;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

import it.unipi.dsmt.fitconnect.services.NodeMessageService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

@Component
public class ErlangNodesController {
    private static ErlangNodesController instance;
    private static String myName;
    private static String cookie;
    private static String erlangMessanger;
    private static String erlangNotifier;
    private static String erlangServerMailBox;
    private static List<ErlangNode> erlangNodes;

    @Autowired
    private NodeMessageService nodeMessageService;

    // Generates a Controller that stores all the information in common for all client nodes
    private ErlangNodesController() {
        myName = "erlangController";
        cookie = "dsmt";
        erlangMessanger = "fitMessanger";
        erlangNotifier = "fitNotifier";
        erlangServerMailBox = "server@10.2.1.82";
        erlangNodes = new ArrayList<>();
        pingErlangServer();
    }

    /**
     * Singleton Pattern to return always the same controller
     * @return the controller itself
     */
    public static synchronized ErlangNodesController getInstance() {
        if (instance == null) {
            instance = new ErlangNodesController();
        }
        return instance;
    }

    /**
     * Checks if the erlang server is up and clears its state if it is
     */
    private void pingErlangServer() {
        try {
            OtpNode javaNode = new OtpNode(myName, cookie); // Java node
            OtpMbox javaMail = javaNode.createMbox("erlangControllerMail");
            OtpErlangTuple from = new OtpErlangTuple(new OtpErlangObject[] {
                    javaMail.self(), javaNode.createRef() });
            if (javaNode.ping(erlangServerMailBox, 100)) {
                System.out.println("ERLANG CONTROLLER -> Node is up"); // DEBUG
                // We clean erlang state since Spring server crashed and disconnected all users
                OtpErlangTuple cleanCommand = new OtpErlangTuple(new OtpErlangObject[]{new OtpErlangAtom("clean")});
                OtpErlangObject msg_gen = new OtpErlangTuple(
                    new OtpErlangObject[] {new OtpErlangAtom("$gen_call"), from, cleanCommand}
                    );
                javaMail.send(erlangMessanger, erlangServerMailBox, msg_gen);
            } else
                System.out.println("ERLANG CONTROLLER -> Node is down"); //DEBUG
        } catch (Exception e) {
            e.printStackTrace();
            System.err.println("ERLANG CONTROLLER -> Failed to establish Erlang connection.");
        }
    }

    /**
     * Returns the poistion of the node
     * @param nodeName username of the node that we are trying to find
     * @return index of node seached
     */
    private int findNode(String nodeName){
        for (int i = 0; i < erlangNodes.size(); i++) {
            if (erlangNodes.get(i).getNodeName().equalsIgnoreCase(nodeName)) {
                return i;
            }
        }
        return erlangNodes.size();
    }

    /**
     * Used to instantiate a clientNode and remember it
     * @param username name of the node and username of the person
     */
    public void startErlangNode(String username, List<String> courses) {
        try {
            int index = findNode(username); // Find index to reference
            if (index == erlangNodes.size())
                erlangNodes.add(
                        new ErlangNode(
                                username, courses, cookie,
                                erlangMessanger, erlangNotifier,
                                erlangServerMailBox, nodeMessageService
                        )
                );
            erlangNodes.get(index).incrementConnected();
        } catch (Exception e) {
            e.printStackTrace();
            System.err.println("ERLANG CONTROLLER -> Failed to start a new node");
        }
    }

    public void sendCommandToNode(String nodeName, String nodeCommand){
        try {
            erlangNodes.get(findNode(nodeName)).processCommand(nodeCommand);
        } catch (Exception e) {
            e.printStackTrace();
            System.err.println("ERLANG CONTROLLER -> Failed to find node");
        }     
    }

    public void disconnectNode(String nodeName){
        int index = findNode(nodeName); // Find index to reference
        try {
            erlangNodes.get(index).decrementConnected();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}