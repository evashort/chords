var ac = new (window.AudioContext || window.webkitAudioContext)();

var reverb = Freeverb(ac);
reverb.roomSize = 0.2;
reverb.wet.value = 0.2;
reverb.dry.value = 0.55;
reverb.connect(ac.destination);

var notes = [];

function stopAt(t, note) {
  if (note.expiration > t) {
    safeTime = Math.max(t, ac.currentTime + 0.01);
    release = 0.1;
    note.fader.gain.setTargetAtTime(0, safeTime, 0.25 * release);
    for (let i = 0; i < note.oscillators.length; i++) {
      note.oscillators[i].stop(safeTime + release);
    }
    note.expiration = t;
  }
}

function cancelAt(t, note) {
  if (note.start >= t) {
    stopAt(ac.currentTime, note);
  } else if (note.expiration >= t) {
    for (let i = 0; i < note.oscillators.length; i++) {
      note.oscillators[i].stop(note.stop);
    }
    note.fader.gain.cancelScheduledValues(0);
    note.expiration = note.stop;
  }
}

function muteAt(t) {
  for (let i = 0; i < notes.length; i++) {
    stopAt(t, notes[i]);
  }
}

function addNote(addInstrumentNote, spec) {
  for (let i = 0; i < notes.length; i++) {
    if (notes[i].frequency == spec.f) {
      stopAt(spec.t, notes[i]);
    }
  }

  notes.push(addInstrumentNote(spec.v, spec.t, spec.f));
}

function changeAudio(changes) {
  if (changes.length > 0) {
    for (let i = 0; i < notes.length; i++) {
      cancelAt(changes[0].t, notes[i]);
    }
  }

  for (let i = 0; i < changes.length; i++) {
    if (changes[i].type == "mute") {
      muteAt(changes[i].t);
    } else if (changes[i].type == "piano") {
      addNote(addPianoNote, changes[i]);
    } else if (changes[i].type == "guitar") {
      addNote(addGuitarNote, changes[i]);
    } else if (changes[i].type == "pad") {
      addNote(addPadNote, changes[i]);
    }
  }

  let newNotes = [];
  for (let i = 0; i < notes.length; i++) {
    if (notes[i].expiration > ac.currentTime) {
      newNotes.push(notes[i]);
    }
  }

  notes = newNotes;
}
