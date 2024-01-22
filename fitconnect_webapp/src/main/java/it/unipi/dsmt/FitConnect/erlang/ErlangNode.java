package it.unipi.dsmt.FitConnect.erlang;

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


    public ErlangNode(String username, List<String> courseNames, String cookie, String erlangMessanger, String erlangNotifier, String erlangServerMailbox) throws IOException{
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

    public String getNodeName(){
        return this.username;
    }

    private void sendRequestToNotifier(String mode, String courseName, String personalTrainer, int time){
        OtpErlangAtom msgType = new OtpErlangAtom(mode);
        OtpErlangString course = new OtpErlangString(courseName);
        OtpErlangString trainer = new OtpErlangString(personalTrainer);
        OtpErlangInt delay = new OtpErlangInt(time);
        OtpErlangTuple outMsg = new OtpErlangTuple(new OtpErlangObject[]{msgType, course, trainer, delay});
        OtpErlangObject msg_gen = new OtpErlangTuple(new OtpErlangObject[] {
            new OtpErlangAtom("$gen_call"), this.from, outMsg });
        this.userMail.send(this.erlangNotifier, this.erlangServerMailbox, msg_gen);
    }

    private void sendRequest(OtpErlangTuple msgStructure) throws OtpErlangExit, OtpErlangDecodeException {
        OtpErlangObject msg_gen = new OtpErlangTuple(new OtpErlangObject[] {
                new OtpErlangAtom("$gen_call"), this.from, msgStructure });
        this.userMail.send(this.erlangMessanger, this.erlangServerMailbox, msg_gen);
    }

    public String[] receive() throws OtpErlangExit, OtpErlangDecodeException {
        OtpErlangObject reply = this.userMail.receive(1000);

        if (reply == null){
            return null;
            //return new String[] {"Error", "Server is down"};
        }

        OtpErlangTuple t = (OtpErlangTuple) reply;
        //System.out.println(this.username + "-> Full reply: " + t.toString());
        OtpErlangTuple msg = (OtpErlangTuple) t.elementAt(1);
        System.out.println(this.username + "-> Message: " + msg.toString());
        System.out.println(this.username + "-> Number of elements in message: " + msg.arity());

        OtpErlangAtom msgType = (OtpErlangAtom) msg.elementAt(0);
        //FIX DEPENDING ON LENGTH
        OtpErlangObject content = msg.elementAt(1);

        return new String[] { msgType.toString(), content.toString()};
    }

    public void connectToAllCourses() throws OtpErlangExit, OtpErlangDecodeException {
        OtpErlangString username = new OtpErlangString(this.username);
        OtpErlangAtom msgType = new OtpErlangAtom("connectToAllCourses");
        OtpErlangObject[] coursesArray = this.courseNames.stream().map(OtpErlangString::new).toArray(OtpErlangObject[]::new);
        OtpErlangList courses = new OtpErlangList(coursesArray);
        OtpErlangTuple outMsg = new OtpErlangTuple(new OtpErlangObject[]{this.userMail.self(), msgType, courses, username});
        sendRequest(outMsg);
    }
      
    public void display(String something) throws OtpErlangExit, OtpErlangDecodeException {
        OtpErlangAtom msgType = new OtpErlangAtom(something);
        OtpErlangTuple outMsg = new OtpErlangTuple(new OtpErlangObject[]{msgType});
        sendRequest(outMsg);
    }

    public void joinCourse(String courseName) throws OtpErlangExit, OtpErlangDecodeException {
        OtpErlangAtom msgType = new OtpErlangAtom("connectToCourse");
        OtpErlangString course = new OtpErlangString(courseName);
        OtpErlangString username = new OtpErlangString(this.username);
        OtpErlangTuple outMsg = new OtpErlangTuple(new OtpErlangObject[]{this.userMail.self(), msgType, course, username});
        sendRequest(outMsg);
    }

    public void leaveCourse(String courseName) throws OtpErlangExit, OtpErlangDecodeException {
        OtpErlangAtom msgType = new OtpErlangAtom("exit");
        OtpErlangString course = new OtpErlangString(courseName);
        OtpErlangString username = new OtpErlangString(this.username);
        OtpErlangTuple outMsg = new OtpErlangTuple(new OtpErlangObject[]{msgType, course, username});
        sendRequest(outMsg);
    }

    public void sendMessage(String message, String receiver) throws OtpErlangExit, OtpErlangDecodeException {
        OtpErlangAtom destType = new OtpErlangAtom("course");
        OtpErlangString username = new OtpErlangString(this.username);
        OtpErlangString destination = new OtpErlangString(receiver);
        OtpErlangAtom msgType = new OtpErlangAtom("send");
        OtpErlangString msg = new OtpErlangString(message);
        OtpErlangTuple outMsg = new OtpErlangTuple(new OtpErlangObject[]{msgType, msg, username, destType, destination});
        sendRequest(outMsg);
    }

    public void disconnect() throws OtpErlangExit, OtpErlangDecodeException {
        OtpErlangAtom msgType = new OtpErlangAtom("disconnect");
        OtpErlangString username = new OtpErlangString(this.username);
        OtpErlangTuple outMsg = new OtpErlangTuple(new OtpErlangObject[]{msgType, username});
        sendRequest(outMsg);
    }

    public void startClientNode() {
        ErlangNodeListener myListener = new ErlangNodeListener(this);
        ErlangNodeSender mySender = new ErlangNodeSender(this);
        try {
            myListener.start();
            mySender.start();
            connectToAllCourses();
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
            case "display": // 1 args: String something = (Users, Courses, Clients)
                display(parts[1].trim());
                break;
            case "join": // 1 args: String courseName
                joinCourse(parts[1].trim());
                break;
            case "disconnect": // 0 args
                disconnect();
                break;
            case "leave": // 1 args: String courseName
                leaveCourse(parts[1].trim());
                break;
            case "send": // 3 args: String message, String receiver
                sendMessage(parts[1].trim(), parts[2].trim());
                break;
            case "i": // 2 args: String mode, String courseName, Int time
                sendRequestToNotifier("insert", parts[1].trim(), this.username, Integer.parseInt(parts[2].trim()));
                break;
            case "e": // 2 args: String mode, String courseName, Int time
                sendRequestToNotifier("edit", parts[1].trim(), this.username, Integer.parseInt(parts[2].trim()));
                break;
            case "d": // 2 args: String mode, String courseName, Int time
                sendRequestToNotifier("delete", parts[1].trim(), this.username, 0);
                break;
            default:
                break;
        }
        } catch (Exception e) {
            e.printStackTrace();
            System.err.println("Something failed.");
        }
    }
}