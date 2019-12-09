/*
 *  Copyright (c) 2015 The WebRTC project authors. All Rights Reserved.
 *
 *  Use of this source code is governed by a BSD-style license
 *  that can be found in the LICENSE file in the root of the source
 *  tree.
 */

'use strict';

const startButton = document.getElementById('startButton');
const callButton = document.getElementById('callButton');
const hangupButton = document.getElementById('hangupButton');
callButton.disabled = true;
hangupButton.disabled = true;
startButton.onclick = start;
callButton.onclick = call;
hangupButton.onclick = hangup;

const video1 = document.querySelector('video#video1');
const video2 = document.querySelector('video#video2');
const video3 = document.querySelector('video#video3');

let pc1Local;
let pc1Remote;
const offerOptions = {
    offerToReceiveAudio: 1,
    offerToReceiveVideo: 1
};

function gotStream(stream) {
    video1.srcObject = stream;
    window.localStream = stream;
    callButton.disabled = false;
}

function start() {
    startButton.disabled = true;
    navigator.mediaDevices
        .getUserMedia({
            audio: true,
            video: true
        })
        .then(gotStream)
}

function call() {
    callButton.disabled = true;
    hangupButton.disabled = false;
    const audioTracks = window.localStream.getAudioTracks();
    const videoTracks = window.localStream.getVideoTracks();
    // Create an RTCPeerConnection via the polyfill.
    const servers = null;
    pc1Local = new RTCPeerConnection(servers);
    pc1Remote = new RTCPeerConnection(servers);
    pc1Remote.ontrack = gotRemoteStream1;
    pc1Local.onicecandidate = iceCallback1Local;
    pc1Remote.onicecandidate = iceCallback1Remote;
    console.log('pc1: created local and remote peer connection objects');

    window.localStream.getTracks().forEach(track => pc1Local.addTrack(track, window.localStream));
    console.log('Adding local stream to pc1Local');
    pc1Local
        .createOffer(offerOptions)
        .then(gotDescription1Local, onCreateSessionDescriptionError);
}

function onCreateSessionDescriptionError(error) {
    console.log(`Failed to create session description: ${error.toString()}`);
}

function gotDescription1Local(desc) {
    pc1Local.setLocalDescription(desc);
    console.log(`Offer from pc1Local\n${desc.sdp}`);
    pc1Remote.setRemoteDescription(desc);
    // Since the 'remote' side has no media stream we need
    // to pass in the right constraints in order for it to
    // accept the incoming offer of audio and video.
    pc1Remote.createAnswer().then(gotDescription1Remote, onCreateSessionDescriptionError);
}

function gotDescription1Remote(desc) {
    pc1Remote.setLocalDescription(desc);
    console.log(`Answer from pc1Remote\n${desc.sdp}`);
    pc1Local.setRemoteDescription(desc);
}

function hangup() {
    console.log('Ending calls');
    pc1Local.close();
    pc1Remote.close();
    pc1Local = pc1Remote = null;
    hangupButton.disabled = true;
    callButton.disabled = false;
}

function gotRemoteStream1(e) {
    if (video2.srcObject !== e.streams[0]) {
        video2.srcObject = e.streams[0];
        console.log('pc1: received remote stream');
    }
}


function iceCallback1Local(event) {
    event.candidate.addIceCandidate(pc1Remote);
}

function iceCallback1Remote(event) {
    event.candidate.addIceCandidate(pc1Local);
}
