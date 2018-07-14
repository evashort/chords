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
  if (storageElement.attributes.value.value == "") {
    localStorage.removeItem("storage");
  } else {
    localStorage.setItem("storage", storageElement.attributes.value.value)
  }
}
