function addPadNote(v, t, f) {
  let fader = ac.createGain();
  fader.connect(reverb);

  let lnf = Math.log(f);
  let peakScale = (0.0529162 * lnf - 0.785209) * lnf + 3.57215
  let decayScale = 0.15;

  let attack = 0.017;
  let peakTime = t + attack;
  let sawPeak = 0.3 * peakScale;
  let squarePeak = 0.5 * sawPeak;
  let stop = t + 30;

  let filter = ac.createBiquadFilter();
  filter.connect(fader);
  filter.frequency.value = 402;
  filter.detune.setValueAtTime(756, t);
  filter.detune.setTargetAtTime(0, peakTime, 2 * decayScale);

  let sawGain = ac.createGain();
  sawGain.connect(filter);
  sawGain.gain.setValueAtTime(0, t);
  sawGain.gain.linearRampToValueAtTime(sawPeak, peakTime);

  let saw1 = ac.createOscillator();
  saw1.type = "sawtooth";
  saw1.frequency.value = 1.0035 * f;
  saw1.connect(sawGain);
  saw1.start(t);
  saw1.stop(stop);

  let saw2 = ac.createOscillator();
  saw2.type = "sawtooth";
  saw2.frequency.value = 0.9965 * f;
  saw2.connect(sawGain);
  saw2.start(t);
  saw2.stop(stop);

  let squareGain = ac.createGain();
  sawGain.connect(filter);
  sawGain.gain.setValueAtTime(0, t);
  sawGain.gain.linearRampToValueAtTime(squarePeak, peakTime);

  let square = ac.createOscillator();
  square.type = "square";
  square.frequency.value = f;
  square.connect(squareGain);
  square.start(t);
  square.stop(stop);

  let note = {
    fader: fader,
    oscillators: [saw1, saw2, square],
    start: t,
    stop: stop,
    expiration: stop,
    frequency: f
  }

  return note;
}
