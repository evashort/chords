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

  var rect = event.target.getBoundingClientRect();
  dragPos = {
    x: (event.clientX - rect.left) / rect.width,
    y: (event.clientY - rect.top) / rect.height
  }
}

var translationRegex = /translate\s*\(\s*([^\s,)]+)[ ,]/;

function harpDragged(event) {
  if (dragPos != null) {
    var harp = document.getElementById("harp");
    if (harp != null) {
      var rect = harp.getBoundingClientRect();

      var oldPos = dragPos;
      dragPos = {
        x: (event.clientX - rect.left) / rect.width,
        y: (event.clientY - rect.top) / rect.height
      }

      var upper = oldPos;
      var lower = dragPos;
      if (dragPos.y < oldPos.y) {
        upper = dragPos;
        lower = oldPos;
      }

      if (upper.y < 1 && lower.y > 0) {
        var highX = upper.x;
        if (upper.y < 0) {
          highX =
            (upper.x * lower.y - lower.x * upper.y) /
              (lower.y - upper.y);
        }

        var lowX = lower.x;
        if (lower.y > 1) {
          lowX =
            (upper.x * (lower.y - 1) - lower.x * (upper.y - 1)) /
              (lower.y - upper.y);
        }

        var viewBox = harp.viewBox.baseVal;

        var xStart = viewBox.x + Math.min(lowX, highX) * viewBox.width;
        var xStop = viewBox.x + Math.max(lowX, highX) * viewBox.width;

        var pitches = [];

        var ropes = harp.getElementsByClassName("rope");
        for (var i = 0; i < ropes.length; i++) {
          var transform = ropes[i].getAttribute("transform");
          var coordinates = translationRegex.exec(transform);
          var x = parseFloat(coordinates[1]);
          if (x >= xStart && x < xStop) {
            var pitch = parseInt(ropes[i].getAttribute("name"));
            pitches.push(pitch);
          }
        }

        if (pitches.length > 0) {
          app.ports.harpPlucked.send(
            { now: ac.currentTime,
              pitches: pitches
            }
          );
        }
      }
    }
  }
}

function harpReleased(event) {
  dragPos = null;
}

window.addEventListener("mousemove", harpDragged);

window.addEventListener("mouseup", harpReleased);
