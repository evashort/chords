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

var harpExists = false;
var dragPos = null;
var plucks = {};
var pulls = {};
var ropeRadius = 2;
var maxExtraLength = 1;

function setHarpExistence(newHarpExistence) {
  if (newHarpExistence != harpExists) {
    harpExists = newHarpExistence;

    if (harpExists) {
      withElement("harp", initHarp);
    }
  }
}

function initHarp(harp) {
  harp.addEventListener("mousedown", harpClicked);
  dragPos = null;
}

function harpClicked(event) {
  event.preventDefault();

  var harp = document.getElementById("harp");
  if (harp == null) {
    return;
  }

  var rect = harp.getBoundingClientRect();
  var viewBox = harp.viewBox.baseVal;
  dragPos = {
    x: viewBox.x + (event.clientX - rect.left) * viewBox.width / rect.width,
    y: (event.clientY - rect.top) * viewBox.height / rect.height
  }
}

var translationRegex = /translate\s*\(\s*([^\s,)]+)[ ,)]/;

function harpDragged(event) {
  if (dragPos == null) {
    return;
  }

  var harp = document.getElementById("harp");
  if (harp == null) {
    return;
  }

  var rect = harp.getBoundingClientRect();
  var viewBox = harp.viewBox.baseVal;
  var height = viewBox.height;

  var oldDragPos = dragPos;
  dragPos = {
    x: viewBox.x + (event.clientX - rect.left) * viewBox.width / rect.width,
    y: (event.clientY - rect.top) * viewBox.height / rect.height
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

    var ropes = harp.getElementsByClassName("rope");
    for (var i = 0; i < ropes.length; i++) {
      var pitch = parseInt(ropes[i].getAttribute("name"));
      if (pitch in pulls) {
        var pull = pulls[pitch];
        if (
          (pull.right && dragPos.x < pull.x) ||
            (!pull.right && dragPos.x > pull.x)
        ) {
          pull.rope.setAttribute(
            "d",
            [Path.bigM(0, 0), Path.bigV(height)].join(" ")
          );

          delete pulls[pitch];
        }

        continue;
      }

      var transform = ropes[i].getAttribute("transform");
      var coordinates = translationRegex.exec(transform);
      var x = parseFloat(coordinates[1]);
      var leftX = x - ropeRadius;
      var rightX = x + ropeRadius;
      if (oldInsidePos.x <= leftX && insidePos.x > leftX) {
        muteCandidates.push(pitch);

        pulls[pitch] = {
          rope: ropes[i],
          x: leftX,
          right: true
        }

        delete plucks[pitch];
      } else if (oldInsidePos.x >= rightX && insidePos.x < rightX) {
        muteCandidates.push(pitch);

        pulls[pitch] = {
          rope: ropes[i],
          x: rightX,
          right: false
        }

        delete plucks[pitch];
      }
    }
  }

  var wasEmpty = true;
  for (var pitch in plucks) {
    wasEmpty = false;
    break;
  }

  var pitches = [];
  var now = performance.now();

  for (var pitch in pulls) {
    var pull = pulls[pitch];

    var yRadius = 0.5 * (height + maxExtraLength);
    var xRadius =
      Math.sqrt(yRadius * yRadius - 0.25 * height * height);

    var x1 = (oldDragPos.x - pull.x) / xRadius;
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
        rope: pull.rope,
        maxStretch: pull.right ? xRadius : -xRadius,
        stretch: (1 - t) * oldDragPos.x + t * dragPos.x - pull.x,
        y: (1 - t) * oldDragPos.y + t * dragPos.y,
        start: now
      };

      pull.rope.setAttribute("fill-opacity", "0.5");

      delete pulls[pitch];
      continue;
    }

    var d = [
      Path.bigM(0, 0),
      Path.l(dragPos.x - pull.x, dragPos.y),
      Path.bigL(0, height),
    ].join(" ");
    pull.rope.setAttribute("d", d);
    pull.rope.setAttribute("fill-opacity", "0");
  }

  if (wasEmpty && pitches.length > 0) {
    updatePlucks(now);
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
}

function harpReleased() {
  dragPos = null;

  var harp = document.getElementById("harp");
  if (harp == null) {
    return;
  }

  var height = harp.viewBox.baseVal.height;

  for (pitch in pulls) {
    pulls[pitch].rope.setAttribute(
      "d",
      [Path.bigM(0, 0), Path.bigV(height)].join(" ")
    );
  }

  pulls = {};
}

function harpMuted() {
  var harp = document.getElementById("harp");
  if (harp == null) {
    return;
  }

  var height = harp.viewBox.baseVal.height;

  for (pitch in plucks) {
    plucks[pitch].rope.setAttribute(
      "d",
      [Path.bigM(0, 0), Path.bigV(height)].join(" ")
    );
  }

  plucks = {};
}

window.addEventListener("mousemove", harpDragged);

window.addEventListener("mouseup", harpReleased);

function updatePlucks(now) {
  var harp = document.getElementById("harp");
  if (harp == null) {
    return;
  }

  var height = harp.viewBox.baseVal.height;

  for (var pitch in plucks) {
    var pluck = plucks[pitch];

    if (pluck.rope.parentNode == null) {
      delete plucks[pitch];
      continue;
    }

    var t = now - pluck.start;
    var stretchPhase = Math.exp(-0.0005 * t);
    var yPhase = Math.exp(-0.005 * t);
    var cornerPhase = yPhase;
    if (t > 6000) {
      pluck.rope.setAttribute(
        "d",
        [Path.bigM(0, 0), Path.bigV(height)].join(" ")
      );

      delete plucks[pitch];
      continue;
    }

    // fudge the stretch to make strings plucked at the end have the
    // same asymptotic decay behavior as those plucked in the middle
    var fudgeStretch =
      pluck.stretch * yPhase + pluck.maxStretch * (1 - yPhase);
    var stretch = fudgeStretch * stretchPhase;
    var y = pluck.y * yPhase + 0.5 * height * (1 - yPhase);
    var curveLeft = stretch * cornerPhase;
    var curveTop = y * cornerPhase;

    var curvePhase = 1 - cornerPhase;
    var curveHeight = height * curvePhase;
    var tightness = 8 / (3 * Math.PI); // this value is closest to a sinusoid
    var controlWidth = stretch * curvePhase * tightness;
    var upperControlHeight = y * curvePhase * tightness;
    var lowerControlHeight = (height - y) * curvePhase * tightness;

    var d = [
      Path.bigM(0, 0),
      Path.l(curveLeft, curveTop),
      Path.c(
        controlWidth, upperControlHeight,
        controlWidth, curveHeight - lowerControlHeight,
        0, curveHeight
      ),
      Path.bigL(0, height),
      Path.l(-curveLeft, -curveTop),
      Path.c(
        -controlWidth, -upperControlHeight,
        -controlWidth, lowerControlHeight - curveHeight,
        0, -curveHeight
      ),
      Path.bigZ()
    ].join(" ");
    pluck.rope.setAttribute("d", d);
  }

  for (var pitch in plucks) {
    requestAnimationFrame(updatePlucks);
    break;
  }
}

var Path = {
  bigM: function(x, y) {
    return "M" + x.toString() + "," + y.toString();
  },
  l: function(dx, dy) {
    return "l" + dx.toString() + "," + dy.toString();
  },
  bigL: function(x, y) {
    return "L" + x.toString() + "," + y.toString();
  },
  v: function(dy) {
    return "v" + dy.toString();
  },
  bigV: function(y) {
    return "V" + y.toString();
  },
  c: function(dx1, dy1, dx2, dy2, dx, dy) {
    return "c" +
      dx1.toString() + "," + dy1.toString() + " " +
      dx2.toString() + "," + dy2.toString() + " " +
      dx.toString() + "," + dy.toString();
  },
  bigZ: function() {
    return "Z";
  }
};
