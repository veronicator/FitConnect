package it.unipi.dsmt.fitconnect.entities;

import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@ToString
public class ChatMessage {
    private String sender;
    private String text;
}
