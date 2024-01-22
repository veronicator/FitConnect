package it.unipi.dsmt.FitConnect.erlang;

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

    private ErlangNodesController() {
        myName = "erlangController";
        cookie = "dsmt";
        erlangMessanger = "fitMessanger";
        erlangNotifier = "fitNotifier";
        erlangServerMailBox = "server@a09ac34a7c62";
        erlangNodes = new ErlangNode[10];
        erlangNodeCount = 0;
        pingErlangServer();
    }

    public static synchronized ErlangNodesController getInstance() {
        if (instance == null) {
            instance = new ErlangNodesController();
        }
        return instance;
    }

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

    public void startErlangNode(String username) {
        List<String> courseNames = Arrays.asList();
        // REPLACE TO GET FROM MONGO DB THE COURSES
        if (username.equals("p")){
            courseNames = Arrays.asList("1", "2", "3", "4", "5");
        } else if (username.equals("pl")){
            courseNames = Arrays.asList("2", "3", "5");
        } else if (username.equals("pa")){
            courseNames = Arrays.asList("1", "2", "4");
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

    private boolean nodeExists(String nodeName){
        for (int i = 0; i < erlangNodeCount; i++) {
                if (erlangNodes[i].getNodeName().equalsIgnoreCase(nodeName))
                return true;
        }
        return false;
    }

    public void sendCommandToNode(String nodeName, String nodeCommand) {
        for (int i = 0; i < erlangNodeCount; i++) {
            if (erlangNodes[i].getNodeName().equalsIgnoreCase(nodeName)) {
                //System.out.println("ERLANG CONTROLLER -> Sending command to: " + nodeName);
                erlangNodes[i].processCommand(nodeCommand);
            } else {
                System.out.println("ERLANG CONTROLLER -> Node not found: " + nodeName);
            }
        }
    }
}

