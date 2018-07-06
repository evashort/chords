function addGuitarNote(v, t, f) {
  let fader = ac.createGain();
  fader.connect(reverb);

  let lnf = Math.log(f);
  let peakScale = (0.0529162 * lnf - 0.785209) * lnf + 3.57215
  let decayScale = 0.15;
  let frequencyCenter = 146.8;
  let filterOffset = 0.7 * (f - frequencyCenter);
  let filterScale = 1 * (v - 1) + 1;

  let attack = 0.001;
  let peakTime = t + attack;
  let sawPeak = 0.3 * peakScale;
  let sawDecay = 10;
  let stop = t + attack + sawDecay;

  let filter = ac.createBiquadFilter();
  filter.connect(fader);
  filter.frequency.value =
    Math.max(1, (132 + filterOffset) * filterScale);
  filter.detune.setValueAtTime(2000, t);
  filter.detune.setTargetAtTime(0, peakTime, 4 * decayScale);

  let sawGain = ac.createGain();
  sawGain.connect(filter);
  sawGain.gain.setValueAtTime(0, t);
  sawGain.gain.linearRampToValueAtTime(sawPeak, peakTime);
  sawGain.gain.setTargetAtTime(0, peakTime, sawDecay * decayScale);

  let saw = ac.createOscillator();
  saw.type = "sawtooth";
  saw.frequency.value = f;
  saw.connect(sawGain);
  saw.start(t);
  saw.stop(stop);

  let note = {
    fader: fader,
    oscillators: [saw],
    start: t,
    stop: stop,
    expiration: stop,
    frequency: f
  }

  return note;
}
