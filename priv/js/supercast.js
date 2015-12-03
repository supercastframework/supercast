$(document).ready(init);

var connected = false;
var ws_worker;
var data_port;
var data_proto;

console.log("hello");
function init() {
    if(!("WebSocket" in window)){
        $('#status').append('<p><span style="color: red;">websockets are not supported </span></p>');
        $("#navigation").hide();
    } else {
        $('#status').append('<p><span style="color: green;">websockets are supported </span></p>');
        start_socket_worker();
        //connect();
    };
    $("#connected").hide();
    $("#content").hide();
};

function start_socket_worker() {
    var wsHost = "ws://" + window.location.host + "/websocket";
    ws_worker = new Worker("js/supercast_ws.js");
    ws_worker.onmessage = function(evt) {handle_socket_msg(evt)};
    ws_worker.postMessage({'type': 'connect', 'host': wsHost});
}

function handle_socket_msg(evt) {
    console.log("socket msg: " + evt.data['type']);
    var obj = evt.data;
    if (obj['type'] == 'open') {
        handleOpenEvent();
    } else if (obj['type'] == 'message') {
        handleMessageEvent(obj['value']);
    } else if (obj['type'] == 'close') {
        handleCloseEvent();
    } else if (obj['type'] == 'error') {
        handleErrorEvent(obj['value']);
    }
}

function stop_socket_worker() {
    ws_worker.postMessage({'type': 'close'});
    connected = false;
};

function subscribeEvent() {
    var channel = $("#channel_id").val();
    subscribe(channel);
}
function unsubscribeEvent() {
    var channel = $("#channel_id").val();
    unsubscribe(channel);
}
function subscribe(channel) {
    ws_worker.postMessage({
        'type': 'send_object',
        'value': {
            'from': 'supercast',
            'type': 'subscribe',
            'value': {
                'queryId': 0,
                'channel': channel
            }
        }
    });
}

function unsubscribe(channel) {
    ws_worker.postMessage({
        'type': 'send_object',
        'value': {
            'from': 'supercast',
            'type': 'unsubscribe',
            'value': {
                'queryId': 0,
                'channel': channel
            }
        }
    });
}

function log_in () {
    var user_name = $("#user_name").val();
    var user_pass = $("#user_pass").val();
    var log_in_obj = {
    }
    ws_worker.postMessage({
        'type': 'send_object',
        'value': {
            'from': 'supercast',
            'type': 'authResp',
            'value': {
                'name': user_name,
                'password': user_pass
            }
        }
    });
    console.log(user_name + " " + user_pass);
}

function handleOpenEvent() {
    connected = true;
    showScreen('<span style="color: green;">CONNECTED </span>');
    $("#connected").fadeIn('slow');
    $("#content").fadeIn('slow');
};

function handleCloseEvent() {
    connected = false;
    ws_worker.terminate();
    ws_worker = undefined;
    showScreen('<span style="color: red;">DISCONNECTED </span>');
};

function handleMessageEvent(obj) {
    showScreen('<span style="color: blue;">MESSAGE: ' + JSON.stringify(obj) + '</span>');
    if (obj['from'] == "supercast") {
        if (obj['type'] == "serverInfo") {
            data_port = obj['value']['dataPort'];
            data_proto = obj['value']['dataProto'];
            console.log("server info: " + data_proto + " " + data_port);
        }
    }
};

function handleErrorEvent(data) {
    showScreen('<span style="color: red;">ERROR: ' + data+ '</span>');
};


function showScreen(txt) {
    $('#output').prepend('<p>' + txt + '</p>');
};

function clearScreen() {
    $('#output').html("");
};
