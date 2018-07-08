var ac = new (window.AudioContext || window.webkitAudioContext)();

var reverb = Freeverb(ac);
reverb.roomSize = 0.2;
reverb.wet.value = 0.2;
reverb.dry.value = 0.55;
reverb.connect(ac.destination);

var notes = [];

function muteNoteAt(t, note) {
  let muteTime = ac.currentTime + 0.025;
  if (t > note.start && muteTime < t) {
    muteTime = t;
  }

  let oldMuteTime =
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
      let release = 0.01;
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
  for (let i = 0; i < notes.length; i++) {
    muteNoteAt(t, notes[i]);
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
  for (let i = 0; i < notes.length; i++) {
    cancelNoteAt(t, notes[i]);
  }
}

function addNote(addInstrumentNote, spec) {
  for (let i = 0; i < notes.length; i++) {
    if (notes[i].frequency == spec.f) {
      muteNoteAt(spec.t, notes[i]);
    }
  }

  notes.push(addInstrumentNote(spec.v, spec.t, spec.f));

  if (!noteCollector) {
    noteCollector = setInterval(collectNotes, 10);
  }
}

function collectNotes() {
  for (let i = notes.length - 1; i >= 0; i--) {
    if (notes[i].expiration <= ac.currentTime) {
      for (let j = 0; j < notes[i].oscillators.length; j++) {
        notes[i].oscillators[j].stop();
      }
      notes[i].fader.disconnect();
      notes.pop();
    } else {
      break;
    }
  }

  if (notes.length == 0) {
    clearInterval(noteCollector);
    noteCollector = 0;
  }
}

var noteCollector = 0;

function changeAudio(changes) {
  for (let i = 0; i < changes.length; i++) {
    if (changes[i].type == "mute") {
      muteAt(changes[i].t);
    } else if (changes[i].type == "cancel") {
      for (let j = 0; j < notes.length; j++) {
        cancelAt(changes[i].t, notes[j]);
      }
    } else if (changes[i].type == "piano") {
      addNote(addPianoNote, changes[i]);
    } else if (changes[i].type == "guitar") {
      addNote(addGuitarNote, changes[i]);
    } else if (changes[i].type == "pad") {
      addNote(addPadNote, changes[i]);
    }
  }

  notes.sort(reverseExpiration);
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
