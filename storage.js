var storageElement = null;

function initStorage() {
  storageElement = document.getElementById("storage");
  var observer = new MutationObserver(updateStorage);
  observer.observe(
    storageElement,
    {
      attributes: true,
      attributeFilter: ["value"]
    }
  );

  updateStorage();
}

function updateStorage() {
  var storage = storageElement.getAttribute("value");
  if (storage == "") {
    localStorage.removeItem("storage");
  } else {
    localStorage.setItem("storage", storage)
  }
}
