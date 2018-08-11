function addPadNote(v, t, f) {
  var fader = ac.createGain();
  fader.connect(reverb);

  var lnf = Math.log(f);
  var peakScale = (0.0529162 * lnf - 0.785209) * lnf + 3.57215
  var decayScale = 0.15;

  var attack = 0.017;
  var peakTime = t + attack;
  var sawPeak = 0.44 * peakScale;
  var squarePeak = 0.5 * sawPeak;

  var filter = ac.createBiquadFilter();
  filter.connect(fader);
  filter.frequency.value = 402;
  filter.detune.setValueAtTime(756, t);
  filter.detune.setTargetAtTime(0, peakTime, 2 * decayScale);

  var sawGain = ac.createGain();
  sawGain.connect(filter);
  sawGain.gain.setValueAtTime(0, t);
  sawGain.gain.linearRampToValueAtTime(sawPeak, peakTime);

  var saw1 = ac.createOscillator();
  saw1.type = "sawtooth";
  saw1.frequency.value = 1.0035 * f;
  saw1.connect(sawGain);
  saw1.start(t);

  var saw2 = ac.createOscillator();
  saw2.type = "sawtooth";
  saw2.frequency.value = 0.9965 * f;
  saw2.connect(sawGain);
  saw2.start(t);

  var squareGain = ac.createGain();
  sawGain.connect(filter);
  sawGain.gain.setValueAtTime(0, t);
  sawGain.gain.linearRampToValueAtTime(squarePeak, peakTime);

  var square = ac.createOscillator();
  square.type = "square";
  square.frequency.value = f;
  square.connect(squareGain);
  square.start(t);

  var note = {
    fader: fader,
    oscillators: [saw1, saw2, square],
    start: t,
    expiration: Infinity,
    frequency: f
  }

  return note;
}
