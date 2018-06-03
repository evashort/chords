var theater = null;
var past = [];
var present = null;
var future = [];
var timeout = 0;
var timeoutIsRedo = false;

function createScene(index, frame) {
  let scene = document.createElement("textarea");
  scene.style.position = "absolute";
  scene.style.top = "0";
  scene.style.left = "0";
  scene.style.width = "100%";
  scene.style.height = "100%";
  scene.style.resize = "none";
  scene.style.boxSizing = "border-box";
  scene.style.padding = "10px";
  scene.style.borderWidth = "2px";
  scene.style.margin = "0";
  scene.style.font = "inherit";
  scene.style.background = "transparent";
  scene.style.visibility = "visible";
  scene.spellcheck = false;
  scene.value = frame.text;
  scene.firstFrame = frame;
  scene.lastFrame = null;
  scene.index = index;
  scene.addEventListener("keydown", handleKeydown);
  scene.addEventListener("input", handleInput);
  return scene;
}

function hideScene(scene) {
  scene.style.visibility = "hidden";
}

function showScene(scene) {
  scene.style.visibility = "visible";
}

function initTheater(frame) {
  theater = document.getElementById("theater");
  present = createScene(0, frame);
  theater.appendChild(present);
}

function replace(replacement) {
  for (let i = 0; i < future.length; i++) {
    theater.removeChild(future[i]);
  }
  future = [];

  present.lastFrame = {
    text: present.value,
    selectionStart: replacement.selectionStart,
    selectionEnd: replacement.selectionEnd
  };
  hideScene(present);
  past.push(present);

  let cursorPos = replacement.selectionStart + replacement.text.length;
  let frame = {
    text:
      present.value.substring(0, replacement.selectionStart) +
      replacement.text +
      present.value.substring(replacement.selectionEnd),
    selectionStart: cursorPos,
    selectionEnd: cursorPos
  };
  present = createScene(present.index + 1, frame);
  theater.appendChild(present);
  // Edge ignores selection changes if the textarea isn't
  // part of the DOM yet
  present.selectionStart = frame.selectionStart;
  present.selectionEnd = frame.selectionEnd;
  present.focus()
}

function undoAndReplace(replacement) {
  for (let i = 0; i < future.length; i++) {
    theater.removeChild(future[i]);
  }
  future = [];
  theater.removeChild(present);

  let previous = past[past.length - 1];
  previous.lastFrame = {
    text: previous.value,
    selectionStart: replacement.selectionStart,
    selectionEnd: replacement.selectionEnd
  };

  let cursorPos = replacement.selectionStart + replacement.text.length;
  let frame = {
    text:
      previous.value.substring(0, replacement.selectionStart) +
      replacement.text +
      previous.value.substring(replacement.selectionEnd),
    selectionStart: cursorPos,
    selectionEnd: cursorPos
  };
  present = createScene(present.index, frame);
  theater.appendChild(present);
  present.selectionStart = frame.selectionStart;
  present.selectionEnd = frame.selectionEnd;
  present.focus()
}

function hardUndo() {
  for (let i = 0; i < future.length; i++) {
    theater.removeChild(future[i]);
  }
  future = [];
  theater.removeChild(present);

  present = past.pop();
  present.lastFrame = null;
  showScene(present);
  present.focus()
}

function undo() {
  if (past.length == 0) {
    return;
  }

  hideScene(present);
  future.push(present);

  present = past.pop();
  showScene(present);
  present.selectionStart = present.lastFrame.selectionStart
  present.selectionEnd = present.lastFrame.selectionEnd
  present.focus()
}

function redo() {
  if (future.length == 0) {
    return;
  }

  hideScene(present);
  past.push(present);

  present = future.pop();
  showScene(present);
  present.selectionStart = present.firstFrame.selectionStart
  present.selectionEnd = present.firstFrame.selectionEnd
  present.focus()
}

function timeoutWrapper(f) {
  return function() {
    timeout = 0;
    f();
    app.ports.text.send(present.value);
  };
}

function handleKeydown(event) {
  if (
    (event.which == 90 || (event.which == 89 && !event.shiftKey)) &&
    event.ctrlKey != event.metaKey &&
    !event.altKey
  ) {
    if (event.which == 90 && !event.shiftKey) {
      handleUndo(event);
    } else {
      handleRedo(event);
    }
  }
}

function handleUndo(event) {
  if (
    event.target.index == present.index &&
    event.target.value == event.target.firstFrame.text &&
    timeout == 0
  ) {
    timeout = setTimeout(timeoutWrapper(undo), 5);
    timeoutIsRedo = false;
  }
}

function handleRedo(event) {
  if (
    event.target.index == present.index &&
    event.target.lastFrame != null &&
    event.target.value == event.target.lastFrame.text &&
    timeout == 0
  ) {
    timeout = setTimeout(timeoutWrapper(redo), 5);
    timeoutIsRedo = true;
  }
}

function handleInput(event) {
  if (event.target.index == present.index) {
    if (timeout != 0) {
      // Check that value has changed because Firefox sends
      // an input event even when undo is disabled
      if (
        timeoutIsRedo ?
        event.target.value != event.target.lastFrame.text :
        event.target.value != event.target.firstFrame.text
      ) {
        clearTimeout(timeout);
        timeout = 0;
      }
    }

    app.ports.text.send(event.target.value);
  } else if (event.target.index < present.index) {
    if (event.target.value != event.target.lastFrame.text) {
      document.execCommand("redo", true, null);
    }
  } else if (event.target.index > present.index) {
    if (event.target.value != event.target.firstFrame.text) {
      document.execCommand("undo", true, null);
    }
  }
}
