var _evanshort73$chords$Native_AudioTime = function() {

var now = _elm_lang$core$Native_Scheduler.nativeBinding(function(callback)
{
	callback(_elm_lang$core$Native_Scheduler.succeed(ac.currentTime));
});

return {
	now: now,
};

}();
