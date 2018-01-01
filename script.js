var ac = new (window.AudioContext || window.webkitAudioContext)();

var reverb = Freeverb(ac);
reverb.roomSize = 0.2;
reverb.wet.value = 0.3;
reverb.dry.value = 0.55;
reverb.connect(ac.destination);

var lp = ac.createBiquadFilter();
lp.frequency = 2200;
lp.connect(reverb);

var synth = new Synth(6, ac);
synth.connect(lp);
synth.start();

function changeAudio(changes) {
  for (let i = 0; i < changes.length; i++) {
    if (changes[i].type == "note") {
      synth.noteAt(changes[i].t, changes[i].f, changes[i].decay);
    } else if (changes[i].type == "mute") {
      synth.muteAt(changes[i].t, changes[i].before);
    } else if (changes[i].type == "cancel") {
      synth.ringOutAt(changes[i].t, changes[i].before);
    } else if (changes[i].type == "attack") {
      synth.attack = changes[i].attack;
    } else if (changes[i].type == "peak") {
      synth.peak = changes[i].peak;
    } else if (changes[i].type == "decay") {
      synth.decay = changes[i].decay;
    }
  }
}

function setSelection(selection) {
  let chordBox = document.getElementById("chordBox");
  chordBox.selectionStart = selection[0];
  chordBox.selectionEnd = selection[1];
  chordBox.focus();
  return [chordBox.selectionStart, chordBox.selectionEnd];
}

function checkSelection() {
  let chordBox = document.getElementById("chordBox");
  return [chordBox.selectionStart, chordBox.selectionEnd];
}
