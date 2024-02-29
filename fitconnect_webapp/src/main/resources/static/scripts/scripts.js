var stompClient = null;
var notificationCount = 0;

$(document).ready(function () {
    console.log("Index page is ready");
    connect();

    $("#send").click(function () {
        sendMessage();
    });

    $("#send-private").click(function () {
        sendPrivateMessage();
    });

    $("#notifications").click(function () {
        resetNotificationCount();
    });
});

function connect() {
    var socket = new SockJS('/our-websocket');
    stompClient = Stomp.over(socket);
    stompClient.connect({}, function (frame) {
        console.log('Connected: ' + frame);

        loadNotifications();
        updateNotificationDisplay();

        stompClient.subscribe('/topic/messages', function (message) {
            message = JSON.parse(message.body).content;
            showMessage(message);
            saveMessage(message);
        });

        stompClient.subscribe('/user/topic/private-messages', function (message) {
            message = JSON.parse(message.body).content;
            showMessage(message);
            saveMessage(message);
        });

        stompClient.subscribe('/topic/global-notifications', function (message) {
            notificationCount = notificationCount + 1;
            updateNotificationDisplay();
        });

        stompClient.subscribe('/user/topic/private-notifications', function (message) {
            notificationCount = notificationCount + 1;
            updateNotificationDisplay();
        });
    });
}

function showMessage(message) {
    console.log("msg {} " + message)
    $("#messages_notify").append("<p class=\"dropdown-item\">" + message + "</p>");
}

function saveMessage(message) {
    // Salva la notifica nella localStorage
    const notifications = JSON.parse(localStorage.getItem('notifications')) || [];
    notifications.push(message);
    localStorage.setItem('notifications', JSON.stringify(notifications));

}

function sendMessage() {
    console.log("sending message");
    stompClient.send("/ws/message", {}, JSON.stringify({'messageContent': $("#message").val()}));
}

function sendPrivateMessage() {
    console.log("sending private message");
    stompClient.send("/ws/private-message", {}, JSON.stringify({'messageContent': $("#private-message").val()}));
}

function updateNotificationDisplay() {
    if (notificationCount == 0) {
        $('#notifications').hide();
    } else {
        $('#notifications').show();
        $('#notifications').text(notificationCount);
    }
}

function deleteNotifications() {
    $("#messages_notify").empty();
    localStorage.removeItem('notifications');
}

function resetNotificationCount() {
    notificationCount = 0;
    deleteNotifications();
    updateNotificationDisplay();
}

function loadNotifications(){
    $("#messages_notify").empty();
    notificationCount = 0;
    const notifications = JSON.parse(localStorage.getItem('notifications')) || [];
    notifications.forEach(function (message) {
        showMessage(message);
        notificationCount = notificationCount + 1;
    });
}