package it.unipi.dsmt.fitconnect.erlang;

import com.ericsson.otp.erlang.*;

import java.io.IOException;
import java.util.List;

public class ErlangNode {
    private String username;
    private String cookie;
    private String erlangMessanger;
    private String erlangNotifier;
    private String erlangServerMailbox;
    private List<String> courseNames;
    private String mailBoxId;
    private OtpNode userNode;
    private OtpMbox userMail;
    private OtpErlangTuple from;

    // Generates a erlang node used for communication between the erlang server and the user
    public ErlangNode(String username, List<String> courseNames, String cookie, String erlangMessanger, String erlangNotifier, String erlangServerMailbox) throws IOException{
        // All user information and Erlang server information
        this.username = username;
        this.courseNames = courseNames;
        this.cookie = cookie;
        this.erlangMessanger = erlangMessanger;
        this.erlangNotifier = erlangNotifier;
        this.erlangServerMailbox = erlangServerMailbox;

        this.mailBoxId = this.username + "Mail";
        this.userNode = new OtpNode(this.username, this.cookie);
        this.userMail = userNode.createMbox(this.mailBoxId);
        this.from = new OtpErlangTuple(new OtpErlangObject[] {
                this.userMail.self(), this.userNode.createRef() });
    }

    /**
     * Gets the username linked to the node
     * @return Username of the user that owns the node
     */
    public String getNodeName(){
        return this.username;
    }

    /**
     * Sends a message to the fitNotifier to set a new timer
     * @param mode it is either "insert", "edit" or "delete"
     * @param courseName course Id
     * @param time timestamp when the course will be held
     */
    private void sendRequestToNotifier(String mode, String courseName, long time){
        OtpErlangAtom msgType = new OtpErlangAtom(mode);
        OtpErlangString user = new OtpErlangString(this.username);
        OtpErlangString course = new OtpErlangString(courseName);
//        OtpErlangInt delay = new OtpErlangInt(time);
        OtpErlangLong delay = new OtpErlangLong(time);
        OtpErlangTuple outMsg = new OtpErlangTuple(new OtpErlangObject[]{msgType, user, course, delay});
        OtpErlangObject msg_gen = new OtpErlangTuple(new OtpErlangObject[] {
            new OtpErlangAtom("$gen_call"), this.from, outMsg });
        this.userMail.send(this.erlangNotifier, this.erlangServerMailbox, msg_gen);
    }

    /**
     * Sends a message to the fitMessanger
     * @param msgStructure message generated by any other function of the node
     */
    private void sendRequest(OtpErlangTuple msgStructure) throws OtpErlangExit, OtpErlangDecodeException {
        OtpErlangObject msg_gen = new OtpErlangTuple(new OtpErlangObject[] {
                new OtpErlangAtom("$gen_call"), this.from, msgStructure });
        this.userMail.send(this.erlangMessanger, this.erlangServerMailbox, msg_gen);
    }

    /**
     * Receives a message from the fitMessanger or fitNotifier
     * @return an Array of strings that should be used to identify the nature of the message
     */
    public String[] receive() throws OtpErlangExit, OtpErlangDecodeException {
        OtpErlangObject reply = this.userMail.receive(1000);

        if (reply == null){
            return null;
        }

//        OtpErlangObject replyObj = reply; // Assign your reply here

        if (reply instanceof OtpErlangTuple t) {

            // Pattern match to distinguish between the two cases
            if (t.elementAt(0) instanceof OtpErlangRef && t.elementAt(1) instanceof OtpErlangTuple innerTuple) {
                // Example -> {#Ref<p@42de442284e2.1.0.0>,{connection,ok}}
                String operation = innerTuple.elementAt(0).toString();
                String result = innerTuple.elementAt(1).toString();
                return new String[] {operation, result};
            } else if (t.elementAt(0) instanceof OtpErlangAtom atom) {
                // Example -> {userExited,"1","p"}
                String[] values = new String[t.arity()]; // Create an array to hold the string values
                values[0] = atom.toString();
                for (int i = 1; i < t.arity(); i++) {
                    values[i - 1] = t.elementAt(i).toString();
                }
                return values;
            }
        } else {
            System.out.println("Error instantiating the tuple");
            return new String[] {"Error while communicating with server"};
        }
        return new String[] {"Generic Java error"};
    }

    /**
     * Called after collecting all the courses from MongoDb to send to the fitMessanger the clients for the current user
     */
    public void connect() throws OtpErlangExit, OtpErlangDecodeException {
        OtpErlangAtom msgType = new OtpErlangAtom("connect");
        OtpErlangObject[] coursesArray = this.courseNames.stream().map(OtpErlangString::new).toArray(OtpErlangObject[]::new);
        OtpErlangString username = new OtpErlangString(this.username);
        OtpErlangList courses = new OtpErlangList(coursesArray);
        OtpErlangTuple outMsg = new OtpErlangTuple(new OtpErlangObject[]{this.userMail.self(), msgType, courses, username});
        sendRequest(outMsg);
    }
    
    /**
     * Debug function to display clients, users, or courses
     * @param something can either be one of the following: clients, users, or courses
     */
    public void display(String something) throws OtpErlangExit, OtpErlangDecodeException {
        OtpErlangAtom msgType = new OtpErlangAtom(something);
        OtpErlangTuple outMsg = new OtpErlangTuple(new OtpErlangObject[]{msgType});
        sendRequest(outMsg);
    }

    /**
     * Adds a state in the fitMessanger to receive messages and notifications from that course
     * @param courseName name of the course that wants to be joined
     */
    public void joinCourse(String courseName) throws OtpErlangExit, OtpErlangDecodeException {
        OtpErlangAtom msgType = new OtpErlangAtom("joinCourse");
        OtpErlangString course = new OtpErlangString(courseName);
        OtpErlangString username = new OtpErlangString(this.username);
        OtpErlangTuple outMsg = new OtpErlangTuple(new OtpErlangObject[]{msgType, course, username});
        sendRequest(outMsg);
    }

    /**
     * Removes a state in the fitMessanger to stop receiving messages and notifications from that course
     * @param courseName name of the course that wants to be left
     */
    public void leaveCourse(String courseName) throws OtpErlangExit, OtpErlangDecodeException {
        OtpErlangAtom msgType = new OtpErlangAtom("exitCourse");
        OtpErlangString course = new OtpErlangString(courseName);
        OtpErlangString username = new OtpErlangString(this.username);
        OtpErlangTuple outMsg = new OtpErlangTuple(new OtpErlangObject[]{msgType, course, username});
        sendRequest(outMsg);
    }

    /**
     * Sends a broadcast message to a course
     * @param message the text of the message that is being sent
     * @param courseName name of the course that is going to receive the message
     */
    public void sendMessage(String message, String courseName) throws OtpErlangExit, OtpErlangDecodeException {
        OtpErlangAtom msgType = new OtpErlangAtom("sendToCourse");
        OtpErlangString course = new OtpErlangString(courseName);
        OtpErlangString username = new OtpErlangString(this.username);
        OtpErlangString msg = new OtpErlangString(message);
        OtpErlangTuple outMsg = new OtpErlangTuple(new OtpErlangObject[]{msgType, course, username, msg});
        sendRequest(outMsg);
    }

    /**
     * Disconnects the user from the fitMessanger removing the structure of that user
     */
    public void disconnect() throws OtpErlangExit, OtpErlangDecodeException {
        OtpErlangAtom msgType = new OtpErlangAtom("disconnect");
        OtpErlangString username = new OtpErlangString(this.username);
        OtpErlangTuple outMsg = new OtpErlangTuple(new OtpErlangObject[]{msgType, username});
        sendRequest(outMsg);
    }

    /**
     * Starts the node for the user generating 2 threads for the node, a receiver and a sender
     */
    public void startClientNode() {
        ErlangNodeListener myListener = new ErlangNodeListener(this);
        ErlangNodeSender mySender = new ErlangNodeSender(this);
        try {
            myListener.start();
            mySender.start();
            connect();
        } catch (Exception e) {
            e.printStackTrace();
            System.err.println("Something failed.");
        }
    }
    
    // VA RIMOSSO PERCHé GESTITO DA THREAD X OGNI USER
    public void processCommand(String command){
        String[] parts = command.split("-");
        String commandName = parts[0].trim();
        try{
            switch (commandName) {
                case "display" -> // 1 args: String something = (Users, Courses, Clients)
                        display(parts[1].trim());
                case "join" -> // 1 args: String courseName
                        joinCourse(parts[1].trim());
                case "disconnect" -> // 0 args
                        disconnect();
                case "leave" -> // 1 args: String courseName
                        leaveCourse(parts[1].trim());
                case "send" -> // 3 args: String message, String receiver
                        sendMessage(parts[1].trim(), parts[2].trim());
                case "i" -> // 2 args: String mode, String courseName, long time
                        sendRequestToNotifier("insert", parts[1].trim(), Long.parseLong(parts[2].trim()));
                case "e" -> // 2 args: String mode, String courseName, long time
                        sendRequestToNotifier("edit", parts[1].trim(), Long.parseLong(parts[2].trim()));
                case "d" -> // 1 args: String mode, String courseName
                        sendRequestToNotifier("delete", parts[1].trim(), 0);
                default -> {
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            System.err.println("Something failed.");
        }
    }
}