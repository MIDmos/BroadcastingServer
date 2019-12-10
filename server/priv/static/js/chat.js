'use strict';
// AddStream
// CreateOffer
// SetLocalDescription
// Send                         ->  Receive
//                                  SetRemoteDescription
//                                  *AddStream
//                                  createAnswer()
//                              <-  setLocalDescription
// Receive                      <-  send
// SetRemoteDescription
// WebRTC know remote peer config


// Send candidate           ->  Receive
//                              Create new Candidate
//                              pc.addIceCandidate
// Receive                  <-  Send candidate
// Create new Candidatex
// addIceCandidate


// Define elements
const remoteVideo = document.getElementById('remoteVideo');
const localVideo = document.getElementById('localVideo');

const findBtn = document.getElementById('findButton');
const startBtn = document.getElementById('startButton');
const endBtn = document.getElementById('endButton');
const messageBox = document.getElementById('messageBox');


findBtn.disabled = true;
endBtn.disabled = true;
startBtn.disabled = true;
messageBox.innerText="Click Start button to join us.";

function startClick() {
    startBtn.disabled = true;
    findBtn.disabled = false;
    endBtn.disabled = false;
    messageBox.innerText="Click Find button to search for partner";
    // Get local Stream
    navigator.mediaDevices.getUserMedia({audio: true, video: true})
        .then(function (stream) {
            localStream = stream;
            localVideo.srcObject = localStream;
        });
}

function endClick() {
    messageBox.innerText="Click Start button to join us.";
    findBtn.disabled = true;
    endBtn.disabled = true;
    if (pc) pc.close();
    startBtn.disabled = false;
    localStream = null;
    localVideo.srcObject = null;
    remoteStream = null;
    remoteVideo.srcObject = null;
    clearInterval(timer);
}


// Define constraints
const pc_config = {
    iceServers: [
        {urls: "stun:turn-001-hstn.steamstack.io:3478"}
    ]
};

// Declare variables
let remoteStream = null;
let localStream = null;
let pc = null;
let timer = null;

function findNewPartner() {
    findPartner();
    setInterval(sleep, 20000);
    remoteVideo.srcObject = null;
}

startBtn.addEventListener('click', startClick);
findBtn.addEventListener('click', findNewPartner);
endBtn.addEventListener('click', endClick);


function findPartner() {
    socket.send(encodeMessage("find_partner"))
}

// To hibernate socket
function sleep() {
    socket.send(encodeMessage("sleep"))
}

// Define WebSocket
let socket = new WebSocket("ws://" + window.location.host + "/ws");
socket.onopen = function () {
    console.log("Connection established.");
    startBtn.disabled=false;
};
socket.onmessage = function (event) {
    clearInterval(timer);
    try {
        const obj = JSON.parse(event.data);
        if (obj.message === "disconnect") {
            remoteVideo.srcObject=null;
            console.log("Disconnect");
            messageBox.innerText="Disconnected";
            findNewPartner();

        } else if (obj.message === "need_offer") {
            console.log("New member");
            newPC();
            pc.createOffer()
                .then(sdp => pc.setLocalDescription(sdp))
                .then(function () {
                    console.log('localDescription', pc.localDescription);
                    socket.send(JSON.stringify(pc.localDescription));
                });

        } else if (obj.candidate) {
            console.log("Got candidate " + obj);
            const c = new RTCIceCandidate(obj);
            pc.addIceCandidate(c);

        } else if (obj.type === "offer") {
            newPC();
            console.log("Got offer " + obj);
            let sessionDescription = new RTCSessionDescription(obj);
            pc.setRemoteDescription(sessionDescription)
                .then(() => pc.createAnswer())
                .then(sdp => pc.setLocalDescription(sdp))
                .then(function () {
                    socket.send(JSON.stringify(pc.localDescription))
                })

        } else if (obj.reply === "no_parnters") {
            messageBox.innerText="You are alone here. Wait a few seconds.";
            console.log("No partners ");
            timer = setInterval(findPartner, 3000);

        } else if (obj.type === "answer") {
            console.log("Got answer " + obj);
            const sessionDescription = new RTCSessionDescription(obj);
            pc.setRemoteDescription(sessionDescription);

        } else if (obj.reply === "error") {
            console.log("Got error ", obj);
            findPartner();
        }

    } catch (e) {
        if (e instanceof SyntaxError) {
            console.log(event.data);
        } else
            console.error(e);
    }
};


function newPC() {
    pc = new RTCPeerConnection(pc_config);
    pc.addEventListener('icecandidate', onIceCandidate);
    pc.addEventListener('addstream', onRemoteStreamAdded);
    pc.addStream(localStream);
    pc.oniceconnectionstatechange = function () {
        if (pc.iceConnectionState === 'disconnected') {
            remoteVideo.srcObject=null;
            messageBox.innerText="Disconnected";
            console.log('Disconnected');
            findPartner();
        }
    }
}


function onRemoteStreamAdded(event) {
    console.log("RemoteStreamAdded");
    remoteStream = event.stream;
    console.log("StreamNow:", remoteStream);
    remoteVideo.srcObject = remoteStream;
    messageBox.innerText="";
}

// Invoked when got candidate message
function onIceCandidate(event) {
    if (event.candidate) {
        console.log(event.candidate);
        socket.send(JSON.stringify(event.candidate));
        console.log("Candidate sent");
    } else {
        console.log("End of candidates.");
    }
}

function encodeMessage(Message) {
    return JSON.stringify({message: Message})
}