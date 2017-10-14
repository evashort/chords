var ac = new (window.AudioContext || window.webkitAudioContext)();

var lp = ac.createBiquadFilter();
lp.frequency = 1800;
lp.connect(ac.destination);

var synth = new Synth(6, ac);
synth.connect(lp);
synth.start();

function getCurrentTime() {
  return ac.currentTime;
}

function playNote(note) {
  synth.noteAt(note.t, note.f);
}
