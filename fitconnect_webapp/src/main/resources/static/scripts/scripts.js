let stompClient = null;
let isConnected = false;
let notificationCount = 0;

$(document).ready(function () {

    // Check if websocket connection already exists
    if (!stompClient) {
        connect();
    } else {
        if (!isConnected) {
            reconnect();
        }
    }

    $("#notifications").click(function () {
        resetNotificationCount();
    });
});

function connect() {
    const socket = new SockJS('/chat-websocket');
    stompClient = Stomp.over(socket);
    stompClient.connect({}, function (frame) {
        console.log('Connected: ' + frame);
        isConnected = true;     // set connection state

        setConnection();
    });
}

function setConnection() {
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

    //setup chat groups for the user
    chatCoursesIds.forEach(course => {
        const address = '/topic/' + course.toString() + '/chat';
        stompClient.subscribe(address, function (newMessageReceived) {
            addMessageReceived(JSON.parse(newMessageReceived.body));
        });
    });


}

function reconnect() {
    if (stompClient && !isConnected) {
        // If the Stomp client has been created and the connection is not active, try reconnecting
        stompClient.connect({}, function (frame) {
            console.log('Reconnected: ' + frame);
            isConnected = true; // set connection 'active'

            setConnection();
        });
    }
}

function disconnect() {

    isConnected = false;

    if (stompClient != null) {
        stompClient.disconnect();
    }
}

//-------------------------------------------- Notifications --------------------------------------------------------
function showMessage(message) {
    console.log("msg {} " + message)
    $("#messages_notify").append("<p class=\"dropdown-item\" style=\"width: 200px; overflow-wrap: break-word;\">" + message + "</p>");
}

function saveMessage(message) {
    // save notify into localstorage
    const notifications = JSON.parse(localStorage.getItem('notifications')) || [];
    notifications.push(message);
    localStorage.setItem('notifications', JSON.stringify(notifications));

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

function loadNotifications() {
    $("#messages_notify").empty();
    notificationCount = 0;
    const notifications = JSON.parse(localStorage.getItem('notifications')) || [];
    notifications.forEach(function (message) {
        showMessage(message);
        notificationCount = notificationCount + 1;
    });
}


