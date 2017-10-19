function Synth(voiceCount, audioContext) {
  this.audioContext = audioContext;
  this.voices = [];
  for (let i = 0; i < voiceCount; i++) {
    this.voices.push(new Voice(this.audioContext));
  }
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

Synth.prototype.noteAt = function(t, f, peak = 0.5) {
  for (let i = 0; i < this.voices.length; i++) {
    if (this.voices[i].getFrequencyAt(t, true) == f) {
      this.voices[i].noteAt(t, f, peak);
      return;
    }
  }

  let minGain = Infinity;
  let quietestVoice = null;
  for (let i = 0; i < this.voices.length; i++) {
    let gain = this.voices[i].getGainAt(t, true);
    if (gain < minGain) {
      minGain = gain;
      quietestVoice = this.voices[i];
      if (gain <= 0) {
        break;
      }
    }
  }

  quietestVoice.noteAt(t, f, peak);
}

Synth.prototype.muteLoudestNoteAt = function(t) {
    let maxGain = -Infinity;
    let loudestVoice = null;
    for (let i = 0; i < this.voices.length; i++) {
      let gain = this.voices[i].getGainAt(t, true);
      if (gain > maxGain) {
        maxGain = gain;
        loudestVoice = this.voices[i];
      }
    }

    loudestVoice.muteAt(t);
}

Synth.prototype.muteAt = function(t, ignoreJumps = false) {
  for (let i = 0; i < this.voices.length; i++) {
    this.voices[i].muteAt(t, ignoreJumps);
  }
}

Synth.prototype.ringOutAt = function(t, ignoreJumps = false) {
  for (let i = 0; i < this.voices.length; i++) {
    this.voices[i].ringOutAt(t, ignoreJumps);
  }
}
