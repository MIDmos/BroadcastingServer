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

// Declare variables
let remoteStream = null;
let localStream = null;
let pc = null;
let timer = null;

findBtn.disabled = true;
endBtn.disabled = true;
startBtn.disabled = true;
messageBox.innerText = "Click Start button to join us.";

async function startClick() {
    // try {
    //     const stream = await navigator.mediaDevices.getUserMedia({audio: true, video: true});
    //     console.log('Received local stream');
    //     localVideo.srcObject = stream;
    //     localStream = stream;
    //     findBtn.disabled = false;
    // } catch (e) {
    //     alert(`getUserMedia() error: ${e.name}`);
    // }
    startBtn.disabled = true;
    findBtn.disabled = false;
    endBtn.disabled = false;
    messageBox.innerText = "Click Find button to search for partner";
    // Get local Stream
    navigator.mediaDevices.getUserMedia({audio: true, video: true})
        .then(function (stream) {
            messageBox.innerText = "Got stream";
            localStream = stream;
            const videoTracks = localStream.getVideoTracks();
            const audioTracks = localStream.getAudioTracks();
            if (videoTracks.length > 0) {
                console.log(`Using video device: ${videoTracks[0].label}`);
            }
            if (audioTracks.length > 0) {
                console.log(`Using audio device: ${audioTracks[0].label}`);
            }
            localVideo.srcObject = localStream;
        })
        .catch((error) => handleError(error))
}

function handleError(error) {
    if (error.name === 'ConstraintNotSatisfiedError') {
        const v = constraints.video;
        messageBox.innerText = "Video resolution error";
    } else if (error.name === 'PermissionDeniedError') {
        messageBox.innerText = 'Permissions have not been granted to use your camera and ' +
            'microphone, you need to allow the page access to your devices.';
    } else messageBox.innerText = "Can't get video"
}

function endClick() {
    messageBox.innerText = "Click Start button to join us.";
    findBtn.disabled = true;
    endBtn.disabled = true;
    if (pc) pc.close();
    pc = null;
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

function findNewPartner() {
    findPartner();
    remoteVideo.srcObject = null;
}

startBtn.addEventListener('click', startClick);
findBtn.addEventListener('click', findNewPartner);
endBtn.addEventListener('click', endClick);


function findPartner() {
    sendCarefully(encodeMessage("find_partner"))
}

// To hibernate socket
function sleep() {
    sendCarefully(encodeMessage("sleep"))
}

// Define WebSocket
let socket = null;

function newSocket() {
    socket = new WebSocket("ws://" + window.location.host + "/ws");
    socket.onopen = function () {
        endClick();
        console.log("Connection established.");
    };
    socket.onclose = function () {
        console.log("closed");
        newSocket();
        // messageBox.innerText = "Disconnected from server. Refresh page, please."
    };
    socket.onmessage = function (event) {
        clearInterval(timer);
        try {
            const obj = JSON.parse(event.data);
            if (obj.message === "disconnect") {
                remoteVideo.srcObject = null;
                console.log("Disconnect");
                messageBox.innerText = "Disconnected";
                findNewPartner();

            } else if (obj.message === "need_offer") {
                console.log("New member");
                newPC();
                pc.createOffer()
                    .then(sdp => pc.setLocalDescription(sdp))
                    .then(function () {
                        console.log('localDescription', pc.localDescription);
                        sendCarefully(JSON.stringify(pc.localDescription));
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
                        sendCarefully(JSON.stringify(pc.localDescription))
                    })

            } else if (obj.reply === "no_parnters") {
                messageBox.innerText = "You are alone here. Wait a few seconds.";
                console.log("No partners ");
                timer = setInterval(findPartner, 5000);

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
}

newSocket();

function newPC() {
    pc = new RTCPeerConnection(pc_config);
    pc.addEventListener('icecandidate', onIceCandidate);
    pc.addEventListener('addstream', onRemoteStreamAdded);
    pc.addStream(localStream);
    pc.oniceconnectionstatechange = function () {
        if (pc.iceConnectionState === 'disconnected') {
            remoteVideo.srcObject = null;
            messageBox.innerText = "Disconnected";
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
    messageBox.innerText = "";
}

// Invoked when got candidate message
function onIceCandidate(event) {
    if (event.candidate) {
        console.log(event.candidate);
        sendCarefully(JSON.stringify(event.candidate));
        console.log("Candidate sent");
    } else {
        console.log("End of candidates.");
    }
}

function encodeMessage(Message) {
    return JSON.stringify({message: Message})
}

function sendCarefully(data) {
    if(data != null || data !=='null' || data !=='')
        socket.send(data);
}