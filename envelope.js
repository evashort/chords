function Line(t, v, slope) {
  this.type = "line";
  this.t = t;
  this.v = v;
  this.slope = slope;
}

Line.prototype.getValueAt = function(t) {
  if (this.slope == 0) {
    return this.v;
  }
  return this.v + this.slope * (t - this.t);
}

function Curve(t, v, target, timeConstant) {
  this.type = "curve";
  this.t = t;
  this.v = v;
  this.target = target;
  this.timeConstant = timeConstant;
}

Curve.prototype.getValueAt = function(t) {
  let vFactor = Math.exp((t - this.t) / -this.timeConstant);
  return this.v * vFactor + this.target * (1 - vFactor);
}

function Envelope(param, audioContext) {
  this.param = param;
  this.audioContext = audioContext;
  this.segments = [new Line(-Infinity, param.value, 0)];
}

Envelope.prototype.countSegmentsStartedBefore = function(t) {
  let i = 0;
  while (i < this.segments.length && this.segments[i].t < t) {
    i++;
  }
  return i;
}

Envelope.prototype.countStartedSegmentsAt = function(t) {
  let i = 0;
  while (i < this.segments.length && this.segments[i].t <= t) {
    i++;
  }
  return i;
}

Envelope.prototype.truncateAt = function(t, cancelJump = false) {
  if (cancelJump) {
    this.segments.length = this.countSegmentsStartedBefore(t);
  } else {
    this.segments.length = this.countStartedSegmentsAt(t);
  }

  // just garbage collection
  this.segments.splice(
    1,
    Math.max(
      0,
      this.countSegmentsStartedBefore(this.audioContext.currentTime) - 2
    )
  );

  let previousSegment, v;

  let lastSegment = this.segments[this.segments.length - 1];
  if (lastSegment.t == t) {
    previousSegment = this.segments[this.segments.length - 2];
    v = lastSegment.v;
    this.segments[this.segments.length - 1] = new Line(t, v, 0);
  } else {
    previousSegment = lastSegment;
    v = lastSegment.getValueAt(t);
    this.segments.push(new Line(t, v, 0));
  }

  this.param.cancelScheduledValues(t);
  if (previousSegment.type == "line" && previousSegment.slope != 0) {
    this.param.linearRampToValueAtTime(v, t);
  } else {
    this.param.setValueAtTime(v, t);
  }
}

Envelope.prototype.getValueAt = function(t) {
  return this.segments[this.countStartedSegmentsAt(t) - 1].getValueAt(t);
}

Envelope.prototype.line = function(t1, t2, v2, cancelJump = false) {
  this.truncateAt(t1, cancelJump);
  let v1 = this.segments[this.segments.length - 1].v;
  let slope = (v2 - v1) / (t2 - t1);
  this.segments[this.segments.length - 1] = new Line(t1, v1, slope);
  this.segments.push(new Line(t2, v2, 0));
  this.param.linearRampToValueAtTime(v2, t2);
};

Envelope.prototype.curve = function(t1, t2, target, cancelJump = false) {
  this.truncateAt(t1, cancelJump);
  let v1 = this.segments[this.segments.length - 1].v;
  let timeConstant = 0.2 * (t2 - t1);
  this.segments[this.segments.length - 1] = new Curve(t1, v1, target, timeConstant);
  this.param.setTargetAtTime(target, t1, timeConstant);
};

Envelope.prototype.jumpAt = function(t, v2) {
  this.truncateAt(t);
  this.segments[this.segments.length - 1].v = v2;
  this.param.setValueAtTime(v2, t);
}
