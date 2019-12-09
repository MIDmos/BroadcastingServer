'use strict';
// Define elements
const localVideo = document.getElementById('localVideo');
const startBtn = document.getElementById('startButton');
const stopBtn = document.getElementById('stopButton');
const hangupBtn = document.getElementById('hangupButton');
const beginBtn = document.getElementById('beginButton');
const exitBtn = document.getElementById('exitButton');

// Define constraints
const pc_config = {
    iceServers: [
        {urls: "stun:turn-001-hstn.steamstack.io:3478"}
    ]
};

beginBtn.disabled = true;


// Declare variables
let localStream = null;
let pc = null;
let current = -1;

let socket = new WebSocket("ws://" + window.location.host + "/ws");
socket.onopen = function () {
    console.log("Connection established.");
    socket.send(encodeMessage("New channel"));
    setInterval(ping, 20000);
};

function ping() {
    socket.send("m")
}

socket.onclose = function () {
    console.log("Connection closed.");
    socket.send(encodeMessage("Close channel"))
};
socket.onmessage = function (event) {
    try {
        const obj = JSON.parse(event.data);
        if (obj.candidate) {
            console.log("Got candidate");
            const c = new RTCIceCandidate(obj);
            pc.addIceCandidate(c);
        } else if (obj.message === "new") {
            console.log("Got New " + obj);
            newPC();
            pc.createOffer()
                .then(sdp => pc.setLocalDescription(sdp))
                .then(function () {
                    console.log('localDescription', pc.localDescription);
                    console.log('current:', current);
                    socket.send(JSON.stringify(pc.localDescription));
                });
        } else if (obj.type === "answer") {
            console.log("Got answer " + obj);
            const sessionDescription = new RTCSessionDescription(obj);
            pc.setRemoteDescription(sessionDescription);
        }
        // else if (obj.type === "newClient") {
        //     console.log("New client " + obj);
        //     newPC();
        // }
    } catch (e) {
        if (e instanceof SyntaxError) {
            console.log(event.data);
        } else
            console.error(e);
    }
};

// start MediaStream
function startStream() {
    startBtn.disabled = true;
    navigator.mediaDevices.getUserMedia({audio: true, video: true})
        .then(function (stream) {
            localVideo.srcObject = stream;
            localStream = stream;
            newPC();
            stopBtn.disabled = false;
            beginBtn.disabled = false;
        }).catch(e => {
        startBtn.disabled = false;
        stopBtn.disabled = true;
    });
}

startBtn.addEventListener('click', startStream);

function newPC() {
    pc = new RTCPeerConnection(pc_config);
    current++;
    console.log("newPc", current);
    pc.addStream(localStream);
    pc.addEventListener('icecandidate', onIceCandidate);
    // pc.addEventListener('addstream', onRemoteStreamAdded);
    // pc.createOffer()
    //     .then(sdp => pc.setLocalDescription(sdp))
    //     .then(function () {
    //         console.log('localDescription', pc.localDescription);
    //         console.log('current:', current);
    //         socket.send(JSON.stringify(pc.localDescription));
    //     });
}

hangupBtn.addEventListener('click', newPC);


// stop MediaStream
function stopStream() {
    localStream.getTracks().forEach(track => track.stop());
    stopBtn.disabled = true;
    startBtn.disabled = false;
    beginBtn.disabled = true;
}

stopBtn.addEventListener('click', stopStream);


// begin translation
function beginTranslation() {
    beginBtn.disabled = true;
    hangupBtn.disabled = false;
    newPC();
}

beginBtn.addEventListener('click', beginTranslation);

// Exit peer-to-peer network
function exitNetwork() {
    pc.forEach(connection => connection.close());
    beginBtn.disabled = false;
}

exitBtn.addEventListener('click', exitNetwork);


// Invoked when got candidate
function onIceCandidate(event) {
    if (event.candidate) {
        const c = new RTCPeerConnection(event.candidate);
        socket.send(JSON.stringify(event.candidate));
    } else {
        console.log("End of candidates.");
    }
}

function onRemoteStreamAdded(stream) {
    console.log("Not adding remote stream", stream)
}

function encodeMessage(Message) {
    return JSON.stringify({message: Message})
}