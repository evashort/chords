function addPianoNote(t, f) {
  let fader = ac.createGain();
  fader.connect(reverb);

  let attack = 0.01;
  let decay = 3.3;
  let stop = t + attack + decay;
  let timeConstant = 0.15 * decay;

  let lnf = Math.log(f);
  let compensation = (0.0529162 * lnf - 0.785209) * lnf + 3.57215
  let amplitude = 0.5 * compensation;

  let carrierGain = ac.createGain();
  carrierGain.connect(fader);
  carrierGain.gain.setValueAtTime(0, t);
  carrierGain.gain.linearRampToValueAtTime(amplitude, t + attack);
  carrierGain.gain.setTargetAtTime(0, t + attack, timeConstant);

  let carrier = ac.createOscillator();
  carrier.frequency.value = f;
  carrier.connect(carrierGain);
  carrier.start(t);
  carrier.stop(stop);

  let modulatorGain = ac.createGain();
  modulatorGain.connect(carrier.frequency);
  modulatorGain.gain.setValueAtTime(360, t);
  modulatorGain.gain.setTargetAtTime(0, t + attack, 0.8 * timeConstant);

  let modulator = ac.createOscillator();
  modulator.frequency.value = f;
  modulator.connect(modulatorGain);
  modulator.start(t);
  modulator.stop(stop);

  let highModulatorGain = ac.createGain();
  highModulatorGain.connect(modulator.frequency);
  highModulatorGain.gain.value = 200;

  let highModulator = ac.createOscillator();
  highModulator.frequency.value = 4 * f;
  highModulator.connect(highModulatorGain);
  highModulator.start(t);
  highModulator.stop(stop);

  let note = {
    fader: fader,
    oscillators: [carrier, modulator, highModulator],
    start: t,
    stop: stop,
    expiration: stop,
    frequency: f
  }

  return note;
}
