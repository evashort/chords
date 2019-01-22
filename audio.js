var ac = new AudioContext();

function initAudioTimeButton(node) {
  node.addEventListener("mousedown", audioTimeButtonMouseDown);
  node.addEventListener("keydown", audioTimeButtonKeyDown);
}

function audioTimeButtonMouseDown(event) {
  var node = event.currentTarget;
  ac.resume().then(
    function() {
      dispatchClickWithAudioTime(node);
    }
  );
}

function audioTimeButtonKeyDown(event) {
  if (event.key == " " || event.key == "Enter") {
    var node = event.currentTarget;
    ac.resume().then(
      function() {
        dispatchClickWithAudioTime(node);
      }
    );
  }
}

function dispatchClickWithAudioTime(node) {
  var clickWithAudioTime =
    new CustomEvent(
      "clickWithAudioTime",
      { detail: ac.currentTime }
    );
  node.dispatchEvent(clickWithAudioTime);
}

function initAudioTimeInput(node) {
  node.addEventListener("input", audioTimeInputInput);
}

function audioTimeInputInput(event) {
  var node = event.currentTarget;
  ac.resume().then(
    function() {
      dispatchInputWithAudioTime(node);
    }
  );
}

function dispatchInputWithAudioTime(node) {
  var inputWithAudioTime =
    new CustomEvent(
      "inputWithAudioTime",
      {
        detail: {
          value: node.value,
          audioTime: ac.currentTime
        }
      }
    );
  node.dispatchEvent(inputWithAudioTime);
}

var masterFader = ac.createGain();
masterFader.connect(ac.destination);

var reverb = Freeverb(ac);
reverb.roomSize = 0.2;
reverb.wet.value = 0.2;
reverb.dry.value = 0.55;
reverb.connect(masterFader);

var notes = [];
var alarms = [];

function muteNoteAt(t, note) {
  var muteTime = ac.currentTime + 0.04;
  if (t > note.start && muteTime < t) {
    muteTime = t;
  }

  var oldMuteTime =
    note.hasOwnProperty("muteTime") ? note.muteTime : note.expiration;

  if (muteTime < oldMuteTime) {
    if (muteTime <= note.start) {
      note.fader.gain.cancelScheduledValues(0);
      note.fader.gain.value = 0;
      note.muteTime = 0;
      if (!note.hasOwnProperty("oldExpiration")) {
        note.oldExpiration = note.expiration;
      }

      note.expiration = 0;
    } else {
      var release = 0.01;
      note.fader.gain.cancelScheduledValues(muteTime);
      note.fader.gain.setValueAtTime(1, muteTime);
      note.fader.gain.linearRampToValueAtTime(0, muteTime + release);
      note.muteTime = muteTime;
      if (!note.hasOwnProperty("oldExpiration")) {
        note.oldExpiration = note.expiration;
      }

      note.expiration = muteTime + release + 0.01;
    }
  }
}

function muteFrequencyAt(t, f) {
  for (var i = 0; i < notes.length; i++) {
    if (notes[i].frequency == f) {
      muteNoteAt(t, notes[i]);
    }
  }
}

function cancelNoteAt(t, note) {
  if (t <= note.start) {
    muteNoteAt(t, note);
  } else if (note.hasOwnProperty("muteTime") && note.muteTime >= t) {
    note.fader.gain.cancelScheduledValues(0);
    note.fader.gain.value = 1;
    note.expiration = note.oldExpiration;
    delete note.oldExpiration;
    delete note.muteTime;
  }
}

function updateAudio() {
  var wasPlaying = notes.length > 0;

  var now = ac.currentTime;
  for (var i = notes.length - 1; i >= 0; i--) {
    if (notes[i].expiration <= now) {
      for (var j = 0; j < notes[i].oscillators.length; j++) {
        notes[i].oscillators[j].stop();
      }
      notes[i].fader.disconnect();
      notes.pop();
    } else {
      break;
    }
  }

  if (wasPlaying) {
    if (notes.length > 0) {
      updateMeters()
    } else {
      app.ports.playing.send(false);
      clearMeters();
    }
  }

  if (alarms[alarms.length - 1] <= now) {
    app.ports.currentTime.send(now);
  }

  while (alarms[alarms.length - 1] <= now) {
    alarms.pop();
  }

  if (notes.length > 0 || alarms.length > 0) {
    requestAnimationFrame(updateAudio);
  }
}

function changeAudio(changes) {
  var wasPlaying = notes.length > 0;
  var wasUpdating = notes.length > 0 || alarms.length > 0;

  for (var changeIndex = 0; changeIndex < changes.length; changeIndex++) {
    var change = changes[changeIndex];
    switch(change.type) {
      case "mute":
        for (var i = 0; i < notes.length; i++) {
          muteNoteAt(change.t, notes[i]);
        }
        break;
      case "cancel":
        for (var i = 0; i < notes.length; i++) {
          cancelNoteAt(change.t, notes[i]);
        }
        break;
      case "piano":
        muteFrequencyAt(change.t, change.f);
        addPianoNote(change.t, change.f);
        break;
      case "guitar":
        muteFrequencyAt(change.t, change.f);
        addGuitarNote(change.v, change.t, change.f);
        break;
      case "pad":
        muteFrequencyAt(change.t, change.f);
        addPadNote(change.t, change.f);
        break;
      case "noteOff":
        muteFrequencyAt(change.t, change.f);
        break;
      case "alarm":
        alarms.push(change.t);
        break;
      default:
        throw "Unknown change type " + change.type
    }
  }

  notes.sort(reverseExpiration);
  alarms.sort(reverseNumbers);

  if (notes.length > 0 && !wasPlaying) {
    app.ports.playing.send(true);
  }

  if ((notes.length > 0 || alarms.length > 0) && !wasUpdating) {
    updateAudio();
  }
}

function reverseNumbers(a, b) {
  if (a > b) {
    return -1;
  }

  if (a < b) {
    return 1;
  }

  return 0;
}

function reverseExpiration(a, b) {
  if (a.expiration > b.expiration) {
    return -1;
  }

  if (a.expiration < b.expiration) {
    return 1;
  }

  return 0;
}

var currentVolume = 1;

function setVolume(volume) {
  var safeTime = ac.currentTime + 0.04;
  masterFader.gain.setValueAtTime(currentVolume, safeTime);
  masterFader.gain.linearRampToValueAtTime(volume, safeTime + 0.01);
  currentVolume = volume;
}
