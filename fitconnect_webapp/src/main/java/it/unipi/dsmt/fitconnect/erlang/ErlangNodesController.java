package it.unipi.dsmt.fitconnect.erlang;

import com.ericsson.otp.erlang.OtpNode;
import org.springframework.stereotype.Component;

import java.util.Arrays;
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
        erlangServerMailBox = "server@6b6101f06fda";
        erlangNodes = new ErlangNode[10];
        erlangNodeCount = 0;
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
     * Used to instantiate a clientNode and remember it
     * @param username name of the node and username of the person
     */
    public void startErlangNode(String username) {
        List<String> courseNames = Arrays.asList();
        // TODO: REPLACE TO GET FROM MONGO DB THE COURSES
        if (username.equals("p")){
            courseNames = Arrays.asList("1", "2", "3", "4", "5");
        } else if (username.equals("pl")){
            courseNames = Arrays.asList("2", "3", "5");
        } else if (username.equals("pa")){
            courseNames = Arrays.asList("1", "2", "4");
        } else {
            courseNames = Arrays.asList("1", "2", "3");
        }
        try {
            if (!nodeExists(username)){
                //System.out.println("ERLANG CONTROLLER -> Generating node: " + username);
                erlangNodes[erlangNodeCount] = new ErlangNode(username, courseNames, cookie, erlangMessanger, erlangNotifier, erlangServerMailBox);
                erlangNodes[erlangNodeCount].startClientNode();
                erlangNodeCount++;
            } else {
                //System.out.println("ERLANG CONTROLLER -> node: " + username + " already exists");
            }
        } catch (Exception e) {
            e.printStackTrace();
            System.err.println("ERLANG CONTROLLER -> Failed to start a new node");
        }
    }

    /**
     * Checks if a user with that username is already connected
     * @param nodeName username of the node and the user that is trying to connecting
     * @return true if it exists, false otherwise
     */
    private boolean nodeExists(String nodeName){
        for (int i = 0; i < erlangNodeCount; i++) {
                if (erlangNodes[i].getNodeName().equalsIgnoreCase(nodeName))
                return true;
        }
        return false;
    }

    // TODO: REMOVE AND USE THE THREAD SENDER
    public void sendCommandToNode(String nodeName, String nodeCommand) {
        for (int i = 0; i < erlangNodeCount; i++) {
            if (erlangNodes[i].getNodeName().equalsIgnoreCase(nodeName)) {
                //System.out.println("ERLANG CONTROLLER -> Sending command to: " + nodeName);
                erlangNodes[i].processCommand(nodeCommand);
            } else {
                //System.out.println("ERLANG CONTROLLER -> Node not found: " + nodeName);
            }
        }
    }
}

