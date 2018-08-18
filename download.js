var base16Alphabet = "0123456789abcdef";
var base16Values = {}
for (var i = 0; i < base16Alphabet.length; i++) {
  base16Values[base16Alphabet[i]] = i;
}

var base64Alphabet =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

function base16ToBase64(base16) {
  if (base16.length % 2) {
    throw "Base-16 string contains half byte: \"" + base16 + "\"";
  }

  var output = [];
  for (var i = 0; i < base16.length; i += 3) {
    var d1 = base16Values[base16[i]];
    if (i + 1 >= base16.length) {
      output.push(base64Alphabet[d1 << 2]);
      continue;
    }

    var d2 = base16Values[base16[i + 1]];
    output.push(base64Alphabet[(d1 << 2) + (d2 >> 2)]);

    if (i + 2 >= base16.length) {
      output.push(base64Alphabet[(d2 & 3) << 4]);
      continue;
    }

    var d3 = base16Values[base16[i + 2]];
    output.push(base64Alphabet[((d2 & 3) << 4) + d3]);
  }

  output.push("=".repeat(base16.length % 3));

  return output.join("");
}

function download(file) {
  var link = document.createElement("a");
  link.setAttribute(
    "href",
    "data:application/octet-stream;base64," +
      base16ToBase64(file.base16.replace(/ +/g, ""))
  );
  link.setAttribute("download", file.name);

  link.style.display = "none";
  document.body.appendChild(link);

  link.click();

  document.body.removeChild(link);
}
