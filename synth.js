function Synth(voiceCount, audioContext) {
  this.audioContext = audioContext;
  this.voices = [];
  for (let i = 0; i < voiceCount; i++) {
    this.voices.push(new Voice(this.audioContext));
  }
  this.attack = 0;
  this.peak = 0.5;
  this.decay = 1.5;
}

Synth.prototype.connect = function(dst) {
  for (let i = 0; i < this.voices.length; i++) {
    this.voices[i].connect(dst);
  }
}

Synth.prototype.start = function() {
  for (let i = 0; i < this.voices.length; i++) {
    this.voices[i].start();
  }
}

Synth.prototype.noteAt = function(t, f) {
  for (let i = 0; i < this.voices.length; i++) {
    if (this.voices[i].getFrequencyAt(t, false) == f) {
      this.voices[i].noteAt(t, f, this.attack, this.peak, this.decay, true);
      return;
    }
  }

  let minGain = Infinity;
  let quietestVoice = null;
  let peakTime = t + this.attack;
  for (let i = 0; i < this.voices.length; i++) {
    let gain = this.voices[i].getGainAt(peakTime, false);
    if (gain < minGain) {
      minGain = gain;
      quietestVoice = this.voices[i];
      if (gain <= 0) {
        break;
      }
    } else if (gain == minGain) {
      let frequency = this.voices[i].getFrequencyAt(peakTime, false);
      let oldFrequency = quietestVoice.getFrequencyAt(peakTime, false);
      if (frequency < oldFrequency) {
        quietestVoice = this.voices[i];
      }
    }
  }

  quietestVoice.noteAt(t, f, this.attack, this.peak, this.decay);
}

Synth.prototype.muteAt = function(t, ignoreJumps = false) {
  for (let i = 0; i < this.voices.length; i++) {
    this.voices[i].muteAt(t, ignoreJumps);
  }
}

Synth.prototype.ringOutAt = function(t, ignoreJumps = false) {
  for (let i = 0; i < this.voices.length; i++) {
    this.voices[i].ringOutAt(t, ignoreJumps, this.decay);
  }
}
