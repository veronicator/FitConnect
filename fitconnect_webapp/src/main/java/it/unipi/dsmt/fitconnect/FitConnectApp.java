package it.unipi.dsmt.fitconnect;

import it.unipi.dsmt.fitconnect.erlang.ErlangNodesController;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

import java.util.Scanner;

@SpringBootApplication
public class FitConnectApp implements CommandLineRunner {

    @Autowired
    private ErlangNodesController controller;
    public static void main(String[] args) {
        SpringApplication.run(FitConnectApp.class, args);
    }

    @Override
    public void run(String... args) {
        Scanner scanner = new Scanner(System.in);
        String command;

        while (true) {
            System.out.print("Enter command (type 'exit' to stop): ");
            command = scanner.nextLine();

            if ("exit".equalsIgnoreCase(command)) {
                System.out.println("Exiting application...");
                break;
            }
            processCommand(command);
        }
        scanner.close();
    }

    private void processCommand(String command) {
        String[] parts = command.split(":");

        if (parts.length == 1) {
            String nodeName = parts[0].trim();
            controller.startErlangNode(nodeName);
        } else if (parts.length == 2) {
            String nodeName = parts[0].trim();
            String nodeCommand = parts[1].trim();
            controller.sendCommandToNode(nodeName, nodeCommand);
        } else {
            System.out.println("APPLICATION -> Invalid command format. Use 'node_name:command'");
        }
    }
}