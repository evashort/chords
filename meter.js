var meterCanvas = null;

var leftAnalyser = null;
var leftWaveform = null;

var rightAnalyser = null;
var rightWaveform = null;

var tailArea = 1; // in seconds
var decayRatio = null;
var stickTime = 2;

var leftMeter = {
  value: 0,
  peak: 0,
  peakTime: 0,
  stuckValue: 0,
  timeout: 0
};

var rightMeter = {
  value: 0,
  peak: 0,
  peakTime: 0,
  stuckValue: 0,
  timeout: 0
};

var meterUpdating = false;

function initMeter() {
  meterCanvas = document.getElementById("meter");

  var splitter = ac.createChannelSplitter();
  masterFader.connect(splitter);

  leftAnalyser = ac.createAnalyser();
  leftAnalyser.fftSize = 2048;
  splitter.connect(leftAnalyser,0,0);
  leftWaveform = new Float32Array(leftAnalyser.frequencyBinCount);

  rightAnalyser = ac.createAnalyser();
  rightAnalyser.fftSize = 2048;
  splitter.connect(rightAnalyser,1,0);
  rightWaveform = new Float32Array(rightAnalyser.frequencyBinCount);

  decayRatio = Math.exp(-1 / (ac.sampleRate * tailArea));

  paintMeters();
}

function updateMeters() {
  var now = ac.currentTime;
  leftAnalyser.getFloatTimeDomainData(leftWaveform);
  rightAnalyser.getFloatTimeDomainData(rightWaveform);
  leftWaveform.reverse();
  rightWaveform.reverse();

  updateMeter(now, leftWaveform, leftMeter);
  updateMeter(now, rightWaveform, rightMeter);

  paintMeters();
}

function clearMeters() {
  leftMeter.value = 0;
  rightMeter.value = 0;

  paintMeters();
}

function updateMeter(now, waveform, meter) {
  var oldDecay = Math.exp((meter.peakTime - now) / tailArea);
  var oldValue = meter.peak * oldDecay;

  meter.value = oldValue;
  // waveform[0] is at time (now - 1 / sampleRate)
  var decay = decayRatio;
  for (var i = 0; i < waveform.length; i++) {
    var sample = Math.abs(waveform[i]);
    if (sample > meter.value) {
      meter.value = sample;
    }

    var value = sample * decay;
    if (value > oldValue) {
      oldValue = value;
      meter.peak = sample;
      meter.peakTime = now - (i + 1) / ac.sampleRate;
    }

    decay *= decayRatio;
  }

  if (meter.value >= meter.stuckValue) {
    meter.stuckValue = meter.value;

    if (meter.timeout != 0) {
      clearTimeout(meter.timeout);
      meter.timeout = 0;
    }
  }

  if (meter.timeout == 0) {
    meter.timeout = setTimeout(unstickMeter, 1000 * stickTime, meter);
  }
}

function unstickMeter(meter) {
  meter.stuckValue = meter.value;
  meter.timeout = 0;
  paintMeters();
}

function paintMeters() {
  var dc = meterCanvas.getContext("2d");
  dc.fillStyle = "black";
  dc.fillRect(0, 0, 100, 5);
  dc.fillRect(0, 6, 100, 5);
  dc.fillStyle = leftMeter.value > 1 ? "red" : "lime";
  dc.fillRect(0, 0, 100 * Math.min(1, leftMeter.value), 5);
  dc.fillStyle = rightMeter.value > 1 ? "red" : "lime";
  dc.fillRect(0, 6, 100 * Math.min(1, rightMeter.value), 5);
  var peakWidth = 3;
  dc.fillStyle = leftMeter.stuckValue > 1 ? "red" : "lime";
  dc.fillRect(
    100 * Math.min(1, leftMeter.stuckValue) - peakWidth, 0, peakWidth, 5
  );
  dc.fillStyle = rightMeter.stuckValue > 1 ? "red" : "lime";
  dc.fillRect(
    100 * Math.min(1, rightMeter.stuckValue) - peakWidth, 6, peakWidth, 5
  );
}
