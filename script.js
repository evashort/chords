var ac = new (window.AudioContext || window.webkitAudioContext)();

var lp = ac.createBiquadFilter();
lp.frequency = 1800;
lp.connect(ac.destination);

var synth = new Synth(6, ac);
synth.connect(lp);
synth.start();

function changeAudio(changes) {
  for (let i = 0; i < changes.length; i++) {
    if (changes[i].type == "note") {
      synth.noteAt(changes[i].t, changes[i].f);
    } else if (changes[i].type == "muteLoudest") {
      synth.muteLoudestNoteAt(changes[i].t);
    } else if (changes[i].type == "mute") {
      synth.muteAt(changes[i].t, changes[i].before);
    } else if (changes[i].type == "cancel") {
      synth.ringOutAt(changes[i].t, changes[i].before);
    }
  }
}

var checkpoints = [];
var nextCheckpoint = Infinity;
var checkpointCallback;

function monitorCheckpoints(timestamp) {
  let now = ac.currentTime;

  if (now >= nextCheckpoint) {
    nextCheckpoint = Infinity;

    for (let i = 0; i < checkpoints.length; i++) {
      if (checkpoints[i] <= now) {
        checkpoints.splice(i, 1);
        i--;
      } else if (checkpoints[i] < nextCheckpoint) {
        nextCheckpoint = checkpoints[i];
      }
    }

    checkpointCallback(now);
  }

  window.requestAnimationFrame(monitorCheckpoints);
}

function startMonitoringCheckpoints(callback) {
  checkpointCallback = callback;
  window.requestAnimationFrame(monitorCheckpoints);
}

function setCheckpoint(t) {
  checkpoints.push(t);
  if (t < nextCheckpoint) {
    nextCheckpoint = t;
  }
}
