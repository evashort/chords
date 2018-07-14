function addGuitarNote(v, t, f) {
  var fader = ac.createGain();
  fader.connect(reverb);

  var lnf = Math.log(f);
  var peakScale = (0.0529162 * lnf - 0.785209) * lnf + 3.57215
  var decayScale = 0.15;
  var frequencyCenter = 146.8;
  var filterOffset = 0.7 * (f - frequencyCenter);
  var filterScale = 1 * (v - 1) + 1;

  var attack = 0.001;
  var peakTime = t + attack;
  var sawPeak = 0.3 * peakScale;
  var sawDecay = 10;

  var filter = ac.createBiquadFilter();
  filter.connect(fader);
  filter.frequency.value =
    Math.max(1, (132 + filterOffset) * filterScale);
  filter.detune.setValueAtTime(2000, t);
  filter.detune.setTargetAtTime(0, peakTime, 4 * decayScale);

  var sawGain = ac.createGain();
  sawGain.connect(filter);
  sawGain.gain.setValueAtTime(0, t);
  sawGain.gain.linearRampToValueAtTime(sawPeak, peakTime);
  sawGain.gain.setTargetAtTime(0, peakTime, sawDecay * decayScale);

  var saw = ac.createOscillator();
  saw.type = "sawtooth";
  saw.frequency.value = f;
  saw.connect(sawGain);
  saw.start(t);

  var note = {
    fader: fader,
    oscillators: [saw],
    start: t,
    expiration: peakTime + 6,
    frequency: f
  }

  return note;
}
