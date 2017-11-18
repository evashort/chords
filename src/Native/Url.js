var _evanshort73$chords$Native_Url = function() {

function percentDecode(string) {
	try {
		return {
			ctor: "Just",
			_0: decodeURIComponent(string)
		};
	} catch(e) {
		return {
			ctor: "Nothing"
		};
	}
}

return {
	percentEncode: encodeURIComponent,
	percentDecode: percentDecode
};

}();
