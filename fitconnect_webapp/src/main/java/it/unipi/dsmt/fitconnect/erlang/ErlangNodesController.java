package it.unipi.dsmt.fitconnect.erlang;

import com.ericsson.otp.erlang.OtpNode;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class ErlangNodesController {
    private static ErlangNodesController instance;
    private static String myName;
    private static String cookie;
    private static String erlangMessanger;
    private static String erlangNotifier;
    private static String erlangServerMailBox;
    private static ErlangNode[] erlangNodes;
    private static int erlangNodeCount;

    // Generates a Controller that stores all the information in common for all client nodes
    private ErlangNodesController() {
        myName = "erlangController";
        cookie = "dsmt";
        erlangMessanger = "fitMessanger";
        erlangNotifier = "fitNotifier";
        erlangServerMailBox = "server@10.2.1.82";
        erlangNodeCount = 10;
        erlangNodes = new ErlangNode[erlangNodeCount];
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
     * Checks if the erlang server is up and prints a message accordingly
     */
    private void pingErlangServer() {
        try {
            OtpNode javaNode = new OtpNode(myName, cookie); // Java node
            if (javaNode.ping(erlangServerMailBox, 100)) {
                System.out.println("ERLANG CONTROLLER -> Node is up");
            } else
                System.out.println("ERLANG CONTROLLER -> Node is down");
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
        for (int i = 0; i < erlangNodeCount; i++) {
            if (erlangNodes[i].getNodeName().equalsIgnoreCase(nodeName)) {
                return i;
            }
        }
        return -1;
    }

    /**
     * Returns the first empty poistion for a new node
     * @return index of empty node
     */
    private int findEmpty(){
        for (int i = 0; i < erlangNodeCount; i++) {
            if (erlangNodes[i] == null) {
                return i;
            }
        }
        return -1;
    }

    /**
     * Used to instantiate a clientNode and remember it
     * @param username name of the node and username of the person
     */
    public void startErlangNode(String username, List<String> courses) {
        try {
            if (findNode(username) == -1){
                int index = findEmpty();
                if (index == -1 )
                    throw new Exception("Max nodes reached");
                erlangNodes[index] = new ErlangNode(username, courses, cookie, erlangMessanger, erlangNotifier, erlangServerMailBox);
                erlangNodes[index].startClientNode();
            }
        } catch (Exception e) {
            e.printStackTrace();
            System.err.println("ERLANG CONTROLLER -> Failed to start a new node");
        }
    }

    public void sendCommandToNode(String nodeName, String nodeCommand){
        erlangNodes[findNode(nodeName)].processCommand(nodeCommand);
    }

    public void disconnectNode(String nodeName){
        int index = findNode(nodeName); // Find index to reference
        erlangNodes[index].processCommand("disconnect"); // Disconnect node
        erlangNodes[index] = null; // Empty the reference
    }
}