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

Voice.prototype.noteAt = function(t, f, peak = 0.5) {
  this.fEnv.jumpAt(t, f);
  this.env.jumpAt(t, peak);
  this.env.curve(t, t + 1.5, 0);
}

Voice.prototype.muteAt = function(t) {
  this.fEnv.truncateAt(t, true);
  this.env.line(t, t + 0.002, 0, true);
}

Voice.prototype.ringOutAt = function(t) {
  this.fEnv.truncateAt(t, true);
  this.env.curve(t, t + 1.5, 0, true);
}

Voice.prototype.getGainAt = function(t, ignoreJumps = false) {
  return this.env.getValueAt(t, ignoreJumps);
}

Voice.prototype.getFrequencyAt = function(t, ignoreJumps = false) {
  return this.fEnv.getValueAt(t, ignoreJumps);
}
