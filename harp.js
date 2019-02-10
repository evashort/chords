var storageElement = null;

function withElement(id, f, timeout) {
  if (typeof timeout === "undefined") {
    timeout = 100;
  }

  let element = document.getElementById(id);
  if (element == null) {
    var expiration = performance.now() + timeout;

    requestAnimationFrame(
      function(now) {
        withElementHelp(id, f, expiration, now);
      }
    );
  } else {
    f(element);
  }
}

function withElementHelp(id, f, expiration, now) {
  var element = document.getElementById(id);

  if (element == null) {
    if (now < expiration) {
      requestAnimationFrame(
        function(now) {
          withElementHelp(id, f, expiration, now);
        }
      );
    } else {
      console.log("Gave up waiting for \"" + id + "\" to appear")
    }
  } else {
    f(element);
  }
}

var harp = null;
var dragPos = null;
var ropes = {}
var pulls = {};
var plucks = {};
var ropeRadius = 2;
var maxExtraLength = 1;

function initHarp() {
  withElement("harp", initHarpHelp);
}

function initHarpHelp(argHarp) {
  harp = argHarp;

  harp.addEventListener("mousedown", harpClicked);

  var observer = new MutationObserver(harpChanged);
  observer.observe(
    harp,
    {
      attributes: true,
      attributeFilter: ["spec"]
    }
  );

  harpChanged();
}

function harpChanged() {
  harpSpec = JSON.parse(harp.getAttribute("spec"));
  ropes = harpSpec.ropes;
  if (harp.width != harpSpec.width) {
    harp.width = harpSpec.width;
  }

  for (var pitch in pulls) {
    if (!(pitch in ropes)) {
      delete pulls[pitch];
    }
  }

  plucks = {};

  paintStaticHarp();
}

function harpClicked(event) {
  event.preventDefault();

  var rect = harp.getBoundingClientRect();
  dragPos = {
    x: (event.clientX - rect.left) * harp.width / rect.width,
    y: (event.clientY - rect.top) * harp.height / rect.height
  }
}

var translationRegex = /translate\s*\(\s*([^\s,)]+)[ ,)]/;

function harpDragged(event) {
  if (dragPos == null || harp == null) {
    return;
  }

  var rect = harp.getBoundingClientRect();
  var height = harp.height;

  var oldDragPos = dragPos;
  dragPos = {
    x: (event.clientX - rect.left) * harp.width / rect.width,
    y: (event.clientY - rect.top) * height / rect.height
  };

  var upper = oldDragPos;
  var lower = dragPos;
  if (dragPos.y < oldDragPos.y) {
    upper = dragPos;
    lower = oldDragPos;
  }

  var muteCandidates = [];

  if (upper.y < height && lower.y > 0) {
    if (upper.y < 0) {
      upper = {
        x:
          (upper.x * lower.y - lower.x * upper.y) /
            (lower.y - upper.y),
        y: 0
      };
    }

    if (lower.y > height) {
      lower = {
        x:
          (upper.x * (lower.y - height) - lower.x * (upper.y - height)) /
            (lower.y - upper.y),
        y: height
      };
    }

    var oldInsidePos = upper;
    var insidePos = lower;
    if (dragPos.y < oldDragPos.y) {
      insidePos = upper;
      oldInsidePos = lower;
    }

    for (var pitch in ropes) {
      var rope = ropes[pitch];
      var left = rope.x - ropeRadius;
      var right = rope.x + ropeRadius;

      if (pitch in pulls) {
        var pulledRight = pulls[pitch];
        if (
          (pulledRight && dragPos.x < left) ||
            (!pulledRight && dragPos.x > right)
        ) {
          delete pulls[pitch];
        }
      } else if (oldInsidePos.x <= left && insidePos.x > left) {
        muteCandidates.push(parseInt(pitch));
        pulls[pitch] = true;
        delete plucks[pitch];
      } else if (oldInsidePos.x >= right && insidePos.x < right) {
        muteCandidates.push(parseInt(pitch));
        pulls[pitch] = false;
        delete plucks[pitch];
      }
    }
  }

  var wasStatic = true;
  for (var pitch in plucks) {
    wasStatic = false;
    break;
  }

  var pitches = [];
  var now = performance.now();

  for (var pitch in pulls) {
    var rope = ropes[pitch];
    var left = rope.x - ropeRadius;
    var right = rope.x + ropeRadius;

    var pulledRight = pulls[pitch];
    var edge = pulledRight ? left : right;

    var yRadius = 0.5 * (height + maxExtraLength);
    var xRadius =
      Math.sqrt(yRadius * yRadius - 0.25 * height * height);

    var x1 = (oldDragPos.x - edge) / xRadius;
    var y1 = (oldDragPos.y - 0.5 * height) / yRadius;

    var dx = (dragPos.x - oldDragPos.x) / xRadius;
    var dy = (dragPos.y - oldDragPos.y) / yRadius;

    var minusC = 1 - x1 * x1 - y1 * y1;
    var halfB = x1 * dx + y1 * dy
    var a = dx * dx + dy * dy

    var quarterDeterminant = halfB * halfB + a * minusC;

    if (quarterDeterminant < 0) {
      throw "Drag path does not intersect ellipse";
    }

    var t = minusC / (halfB + Math.sqrt(quarterDeterminant));
    if (t < 0) {
      throw "Drag path intersects ellipse only in the past";
    }

    if (t < 1) {
      pitches.push(parseInt(pitch));

      plucks[pitch] = {
        maxStretch: pulledRight ? xRadius : -xRadius,
        stretch: (1 - t) * oldDragPos.x + t * dragPos.x - edge,
        y: (1 - t) * oldDragPos.y + t * dragPos.y,
        start: now
      };

      delete pulls[pitch];
    }
  }

  if (pitches.length > 0 || muteCandidates.length > 0) {
    var mutes = [];
    for (var i = 0; i < muteCandidates.length; i++) {
      if (!(muteCandidates[i] in plucks)) {
        mutes.push(muteCandidates[i]);
      }
    }

    app.ports.harpPlucked.send(
      { now: ac.currentTime,
        mutes: mutes,
        pitches: pitches
      }
    );
  }

  if (wasStatic) {
    if (pitches.length > 0) {
      startPaintingHarp(now);
    } else {
      paintStaticHarp();
    }
  }
}

function harpReleased() {
  if (dragPos == null || harp == null) {
    return;
  }

  dragPos = null;
  pulls = {};

  for (pitch in plucks) {
    return;
  }

  paintStaticHarp();
}

window.addEventListener("mousemove", harpDragged);

window.addEventListener("mouseup", harpReleased);

function startPaintingHarp(now) {
  var wasStatic = true;
  for (var pitch in plucks) {
    wasStatic = false;
  }

  if (wasStatic) {
    return;
  }

  var height = harp.height;

  var dc = harp.getContext("2d");
  dc.fillStyle = "#eeeeee";
  dc.fillRect(0, 0, harp.width, height);

  dc.lineWidth = 2 * ropeRadius;

  for (var pitch in ropes) {
    rope = ropes[pitch];
    dc.strokeStyle = rope.color;
    dc.fillStyle = rope.color;

    if (pitch in pulls) {
      dc.beginPath();
      dc.moveTo(rope.x, 0);
      if (pulls[pitch]) {
        dc.lineTo(dragPos.x + ropeRadius, dragPos.y);
      } else {
        dc.lineTo(dragPos.x - ropeRadius, dragPos.y);
      }

      dc.lineTo(rope.x, height);
      dc.stroke();

      continue;
    }

    // Cut it just short enough to avoid the chrome rendering glitch
    // where only the fill is visible
    if (pitch in plucks && now - plucks[pitch].start > 5850) {
      delete plucks[pitch];
    }

    if (!(pitch in plucks)) {
      dc.beginPath();
      dc.moveTo(rope.x, 0);
      dc.lineTo(rope.x, height);
      dc.stroke();

      continue;
    }

    var pluck = plucks[pitch];

    var t = now - pluck.start;
    var stretchPhase = Math.exp(-0.0005 * t);
    var yPhase = Math.exp(-0.005 * t);
    var cornerPhase = yPhase;

    // fudge the stretch to make strings plucked at the end have the
    // same asymptotic decay behavior as those plucked in the middle
    var fudgeStretch =
      pluck.stretch * yPhase + pluck.maxStretch * (1 - yPhase);
    var stretch = fudgeStretch * stretchPhase;
    var y = pluck.y * yPhase + 0.5 * height * (1 - yPhase);
    var curveLeft = stretch * cornerPhase;
    var curveTop = y * cornerPhase;

    var curvePhase = 1 - cornerPhase;
    var curveBottom = height * curvePhase + y * cornerPhase;
    var tightness = 8 / (3 * Math.PI); // this value is closest to a sinusoid
    var controlWidth = stretch * curvePhase * tightness;
    var upperControlHeight = y * curvePhase * tightness;
    var lowerControlHeight = (height - y) * curvePhase * tightness;

    dc.beginPath();
    dc.moveTo(rope.x, 0);
    dc.lineTo(rope.x + curveLeft, curveTop);
    dc.bezierCurveTo(
      rope.x + curveLeft + controlWidth, curveTop + upperControlHeight,
      rope.x + curveLeft + controlWidth, curveBottom - lowerControlHeight,
      rope.x + curveLeft, curveBottom
    );
    dc.lineTo(rope.x, height);
    dc.lineTo(rope.x - curveLeft, height - curveTop);
    dc.bezierCurveTo(
      rope.x - curveLeft - controlWidth,
      height - curveTop - upperControlHeight,
      rope.x - curveLeft - controlWidth,
      height - curveBottom + lowerControlHeight,
      rope.x - curveLeft,
      height - curveBottom
    );
    dc.closePath();
    dc.globalAlpha = 0.5;
    dc.fill();
    dc.globalAlpha = 1;
    dc.stroke();
  }

  var text = "Select piano keys to create strings";
  for (var pitch in ropes) {
    text = "Drag across strings to strum";
  }

  dc.font = "16px Arial, \"Helvetica Neue\", Helvetica, sans-serif";
  dc.fillStyle = "#6d6d6d";
  dc.textAlign = "center";
  dc.fillText(text, 0.5 * harp.width, 0.5 * height + 6);

  requestAnimationFrame(startPaintingHarp);
}

function paintStaticHarp() {
  var height = harp.height;

  var dc = harp.getContext("2d");
  dc.fillStyle = "#eeeeee";
  dc.fillRect(0, 0, harp.width, height);

  dc.lineWidth = 2 * ropeRadius;

  for (var pitch in ropes) {
    rope = ropes[pitch];
    dc.strokeStyle = rope.color;

    if (pitch in pulls) {
      dc.beginPath();
      dc.moveTo(rope.x, 0);
      if (pulls[pitch]) {
        dc.lineTo(dragPos.x + ropeRadius, dragPos.y);
      } else {
        dc.lineTo(dragPos.x - ropeRadius, dragPos.y);
      }

      dc.lineTo(rope.x, height);
      dc.stroke();
    } else {
      dc.beginPath();
      dc.moveTo(rope.x, 0);
      dc.lineTo(rope.x, height);
      dc.stroke();
    }
  }

  var text = "Select piano keys to create strings";
  for (var pitch in ropes) {
    text = "Drag across strings to strum";
  }

  dc.font = "16px Arial, \"Helvetica Neue\", Helvetica, sans-serif";
  dc.fillStyle = "#6d6d6d";
  dc.textAlign = "center";
  dc.fillText(text, 0.5 * harp.width, 0.5 * height + 6);
}
