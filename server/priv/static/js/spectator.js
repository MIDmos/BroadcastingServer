'use strict';
// Define elements
const remoteVideo = document.getElementById('remoteVideo');
const hangupBtn = document.getElementById('hangupButton');
const beginBtn = document.getElementById('beginButton');

// Define constraints
const pc_config = {
    iceServers: [
        {urls: "stun:turn-001-hstn.steamstack.io:3478"}
    ]
};

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

//DONE

// Send candidate           ->  Receive
//                              Create new Candidate
//                              pc.addIceCandidate
// Receive                  <-  Send candidate
// Create new Candidate
// addIceCandidate

// Declare variables
let remoteStream = null;
// let localStream = null;
let pc = null;

let socket = new WebSocket("ws://" + window.location.host + "/ws");
socket.onopen = function () {
    console.log("Connection established.");
    socket.send(encodeMessage("new"))
};
socket.onmessage = function (event) {
    try {
        const obj = JSON.parse(event.data);
        if (obj.candidate) {
            console.log("Got candidate " + obj);
            const c = new RTCIceCandidate(obj);
            pc.addIceCandidate(c);

        } else if (obj.message === "new") {
            console.log("Got New " + obj);
            // const c = new RTCIceCandidate(obj);
            // pc.addIceCandidate(c);

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
        }
    } catch (e) {
        if (e instanceof SyntaxError) {
            console.log(event.data);
        } else
            console.error(e);
    }
};


// begin translation
function beginTranslation() {
    beginBtn.disabled = true;
    hangupBtn.disabled = false;
}

beginBtn.addEventListener('click', beginTranslation);


function newPC() {
    pc = new RTCPeerConnection(pc_config);
    if (remoteStream) pc.addStream(remoteStream);
    pc.addEventListener('icecandidate', onIceCandidate);
    pc.addEventListener('addstream', onRemoteStreamAdded);
    // pc.createOffer()
    //     .then(sdp => pc.setLocalDescription(sdp))
    //     .then(function () {
    //         console.log('local description', pc.localDescription);
    //         socket.send(JSON.stringify(pc.localDescription));
    //     });
}

function onRemoteStreamAdded(event) {
    console.log("RemoteStreamAdded");
    remoteStream = event.stream;
    pc.addStream(remoteStream);
    console.log("StreamNow:", remoteStream);
    remoteVideo.srcObject = remoteStream;
}

// Invoked when got candidate message
function onIceCandidate(event) {
    if (event.candidate) {
        console.log(event.candidate);
        pc.addIceCandidate(event.candidate);
        socket.send(JSON.stringify(event.candidate));
        console.log("Browser candidate sent");
    } else {
        console.log("End of candidates.");
    }
}

function encodeMessage(Message) {
    return JSON.stringify({message: Message})
}