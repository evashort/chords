function addPianoNote(change) {
  var t = change.t
  var f = change.f

  var fader = ac.createGain();
  fader.connect(reverb);

  var lnf = Math.log(f);
  var peakScale = (0.0529162 * lnf - 0.785209) * lnf + 3.57215
  var decayScale = 0.15;
  var frequencyCenter = 146.8;
  var filterOffset = 0.3 * (f - frequencyCenter);

  var safeTime = Math.max(t, ac.currentTime + 0.003);
  var attack = 0.005;
  var peakTime = safeTime + attack;
  var sawPeak = 0.9 * peakScale;
  var sawDecay = 2;

  var filter = ac.createBiquadFilter();
  filter.connect(fader);
  filter.frequency.value = 285 + filterOffset;
  filter.detune.value = 300;
  filter.detune.setTargetAtTime(0, peakTime, 8 * decayScale);

  var sawGain = ac.createGain();
  sawGain.connect(filter);
  sawGain.gain.value = 0;
  sawGain.gain.setValueAtTime(0, safeTime);
  sawGain.gain.linearRampToValueAtTime(sawPeak, peakTime);
  sawGain.gain.setTargetAtTime(0, peakTime, sawDecay * decayScale);

  var saw = ac.createOscillator();
  saw.type = "sawtooth";
  saw.frequency.value = f;
  saw.connect(sawGain);
  saw.start(safeTime);

  var note = {
    fader: fader,
    oscillators: [saw],
    start: t,
    expiration: peakTime + sawDecay,
    frequency: f
  }

  return note;
}
