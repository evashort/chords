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

function setSelection(range) {
  chordBox = document.getElementById("chordBox");
  chordBox.selectionStart = range[0];
  chordBox.selectionEnd = range[1];
  chordBox.focus();
}

function checkSelection() {
  chordBox = document.getElementById("chordBox");
  return [chordBox.selectionStart, chordBox.selectionEnd]
}

function setChordBoxText(text) {
  chordBox = document.getElementById("chordBox");
  chordBox.value = text;
}
