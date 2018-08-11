var ac = new AudioContext();

var masterFader = ac.createGain();
masterFader.connect(ac.destination);

var reverb = Freeverb(ac);
reverb.roomSize = 0.2;
reverb.wet.value = 0.2;
reverb.dry.value = 0.55;
reverb.connect(masterFader);

var notes = [];

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

function muteAt(t) {
  for (var i = 0; i < notes.length; i++) {
    muteNoteAt(t, notes[i]);
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

function cancelAt(t) {
  for (var i = 0; i < notes.length; i++) {
    cancelNoteAt(t, notes[i]);
  }
}

function addNote(addInstrumentNote, spec) {
  muteFrequencyAt(spec.t, spec.f);

  notes.push(addInstrumentNote(spec.v, spec.t, spec.f));
}

function updateAudio() {
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

  if (notes.length == 0) {
    playing = false
    app.ports.playing.send(false);
    clearMeters();
  } else {
    updateMeters()
    requestAnimationFrame(updateAudio);
  }
}

var playing = false;

function changeAudio(changes) {
  for (var i = 0; i < changes.length; i++) {
    if (changes[i].type == "mute") {
      muteAt(changes[i].t);
    } else if (changes[i].type == "cancel") {
      for (var j = 0; j < notes.length; j++) {
        cancelAt(changes[i].t, notes[j]);
      }
    } else if (changes[i].type == "piano") {
      addNote(addPianoNote, changes[i]);
    } else if (changes[i].type == "guitar") {
      addNote(addGuitarNote, changes[i]);
    } else if (changes[i].type == "pad") {
      addNote(addPadNote, changes[i]);
    } else if (changes[i].type == "noteOff") {
      muteFrequencyAt(changes[i].t, changes[i].f);
    }
  }

  notes.sort(reverseExpiration);

  if (notes.length > 0 && !playing) {
    playing = true;
    app.ports.playing.send(true);
    updateAudio();
  }
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

function stopAudio() {
  muteAt(0);
  harpMuted();
}
