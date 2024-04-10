//---------------------------------------------- Chat --------------------------------------------------------------

function addMessageReceived(newMessageReceived) {

    const sender = document.getElementById('sender').value;

    const messagesContainer = document.getElementById('messages-container');

    let newMessagePosition = 'left';

    if (newMessageReceived.sender == sender) {

        newMessagePosition = 'right';
    }

    const newMessage = document.createElement("div");

    newMessage.className = 'new-message ' + newMessagePosition;

    newMessage.innerHTML = `
                    <div class="new-message-box">
                        <div class="text">${newMessageReceived.text}</div>
                        <div class="details-container">${newMessageReceived.sender} - ${formatDate(newMessageReceived.sendTime)}</div>
                    </div>
                `;

    messagesContainer.appendChild(newMessage);
}

function sendMessage() {
    const room = document.getElementById('room').value;
    const sender = document.getElementById('sender').value;
    const text = document.getElementById('text').value;
    const destination = "/chat-app/" + room + "/chat";

    stompClient.send(
        destination, {},
        JSON.stringify({
            'sender': sender,
            'text': text
        })
    );

    document.getElementById('text').value = '';
}

function formatDate(inputDate) {
    // Parsing della data
    let date = new Date(inputDate);

    // Estrazione delle componenti della data
    let day = ("0" + date.getDate()).slice(-2);
    let month = ("0" + (date.getMonth() + 1)).slice(-2);
    let year = date.getFullYear().toString().slice(-2);

    let hours = ("0" + date.getHours()).slice(-2);
    let minutes = ("0" + date.getMinutes()).slice(-2);

    // Costruzione della data formattata
    let formattedDate = `${day}/${month}/${year}, ${hours}:${minutes}`;

    return formattedDate;
}


$(document).ready(function () {
    var pageNumber = 0;
    var room = $('.messages-container').data('room');
    var username = $('.messages-container').data('username');

    function loadMessages(pageNumber) {
        $.ajax({
            url: '/chat/' + room + '/' + pageNumber,
            method: 'GET',
            success: function (data) {
                // Aggiungi i messaggi al contenitore dei messaggi
                var messagesContainer = $('#messages-container');
                if (data !== null && data.length > 0) {
                    data.forEach(function (message) {
                        var messageHtml = `
                            <div class="new-message ${message.sender == username ? 'right' : 'left'}">
                                <div class="new-message-box">
                                    <div class="text">${message.text}</div>
                                    <div class="details-container">${message.sender} - ${formatDate(message.sendTime)}</div>
                                </div>
                            </div>`;
                        messagesContainer.prepend(messageHtml);
                    });
                } else {
                    $('#load-more').hide();
                }
            },
            error: function (xhr, status, error) {
                console.error('Error fetching messages:', error);
            }
        });
    }

    //load first page of messages
    loadMessages(pageNumber);

    // load other messages if required by the user with 'show more' button
    $('#load-more').click(function () {
        pageNumber++;
        loadMessages(pageNumber);
    });
});