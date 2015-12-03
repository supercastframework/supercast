self.addEventListener('message', function(e) {handle_ui_msg(e)}, false);

function handle_ui_msg(evt) {
    if (evt.data['type'] == 'connect') {
        console.log("connect");
        self.wsHost = evt.data['host'];
        self.connect();
    } else if (evt.data['type'] == 'close') {
        self.disconnect();
    } else if (evt.data['type'] == 'send_object') {
        console.log("send object event: " + evt.data);
        self.send_object(evt.data['value']);
    } else if (evt.data['type'] == 'send_text') {
        self.send_text(evt.data['value']);
    }
}

function connect() {
    console.log("connect to: " + self.wsHost);
    self.websocket = new WebSocket(self.wsHost);
    self.websocket.onopen    = function(evt) { onOpen(evt); };
    self.websocket.onclose   = function(evt) { onClose(evt); };
    self.websocket.onmessage = function(evt) { onMessage(evt); };
    self.websocket.onerror   = function(evt) { onError(evt); };
};

function disconnect() {
    console.log("disconnect");
    self.websocket.close();
    self.terminate();
};

function send_object(obj) {
    console.log("send object" + JSON.stringify(obj));
    self.websocket.send(JSON.stringify(obj));
}
function send_text(txt) { self.websocket.send(txt); }

function onOpen(evt) { self.postMessage({'type': 'open'}) };

function onMessage(evt) {
    self.postMessage({'type': 'message', 'value': JSON.parse(evt.data)});
};

function onError(evt) {
    self.postMessage({'type': 'error', 'value': evt.data});
    self.websocket.close();
};

function onClose(evt) {
    self.postMessage({'type': 'close'})
    self.terminate();
};

