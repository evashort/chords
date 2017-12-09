function Voice(audioContext) {
  this.audioContext = audioContext;
  let ac = this.audioContext;

  this.carrierGain = ac.createGain();
  this.carrierGain.gain.value = 0;

  this.carrier = ac.createOscillator();
  this.carrier.frequency.value = 0;
  this.carrier.type = "sawtooth";
  this.carrier.connect(this.carrierGain);

  this.env = new Envelope(this.carrierGain.gain, ac);
  this.fEnv = new Envelope(this.carrier.frequency, ac);
}

Voice.prototype.connect = function(dst) {
  this.carrierGain.connect(dst);
}

Voice.prototype.start = function() {
  this.carrier.start();
}

Voice.prototype.noteAt = function(
  t, f, attack = 0, peak = 0.5, decay = 1.5, sameFrequency = false
) {
  this.fEnv.jumpAt(t, f);
  if (attack > 0) {
    if (!sameFrequency) {
      this.env.jumpAt(t, 0);
    }
    this.env.line(t, t + attack, peak);
  } else {
    this.env.jumpAt(t, peak);
  }
  if (decay < Infinity) {
    this.env.curve(t, t + attack + decay, 0);
  }
}

Voice.prototype.muteAt = function(t, ignoreJumps = false) {
  this.fEnv.truncateAt(t, ignoreJumps);
  this.env.line(t, t + 0.002, 0, ignoreJumps);
}

Voice.prototype.ringOutAt = function(t, ignoreJumps = false, decay = 1.5) {
  this.fEnv.truncateAt(t, ignoreJumps);
  if (decay < Infinity) {
    this.env.curve(t, t + decay, 0, ignoreJumps);
  }
}

Voice.prototype.getGainAt = function(t, ignoreJumps = false) {
  return this.env.getValueAt(t, ignoreJumps);
}

Voice.prototype.getFrequencyAt = function(t, ignoreJumps = false) {
  return this.fEnv.getValueAt(t, ignoreJumps);
}
