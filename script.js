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
      synth.muteLoudestNoteAt(changes[i].t, changes[i].before);
    } else if (changes[i].type == "mute") {
      synth.muteAt(changes[i].t, changes[i].before);
    } else if (changes[i].type == "cancel") {
      synth.ringOutAt(changes[i].t, changes[i].before);
    }
  }
}

var landingPad = null;

function setSelection(selection) {
  let chordBox = document.getElementById("chordBox");
  chordBox.selectionStart = selection.start;
  chordBox.selectionEnd = selection.stop;
  chordBox.focus();
  return {
    landingPad: landingPad,
    selection: {
      start: chordBox.selectionStart,
      stop: chordBox.selectionEnd
    }
  };
}

function setLandingPad(newLandingPad) {
  landingPad = newLandingPad;
  return setSelection(newLandingPad.selection);
}

function removeLandingPad(source) {
  if (landingPad != null && landingPad.source == source) {
    landingPad = null;
    return checkSelection();
  }
  return null;
}

function checkSelection() {
  let chordBox = document.getElementById("chordBox");
  return {
    landingPad: landingPad,
    selection: {
      start: chordBox.selectionStart,
      stop: chordBox.selectionEnd
    }
  };
}

function setChordBoxText(text) {
  let chordBox = document.getElementById("chordBox");
  chordBox.value = text;
}

function focusById(id) {
  let el = document.getElementById(id);
  if (el != null) {
    el.focus();
  }
}
