# chords
text-based chord progression editor

## planned features
- adjustable key signature
- adjustable root octave
- adjustable tempo
- more arpeggio styles
- continuous pad playback mode
- transpose selected chords
- 9th chords
- recently played chord sequences
- 7th chords displayed within circle of 5ths, with show/hide option

## implementation details
On the JS side, I use the Web Audio API to set up 6 sawtooth oscillators, connect each one to a gain node, and use Web Audio API commands like `setTargetAtTime` to schedule frequency and amplitude changes in advance. Unfortunately the API for scheduling future changes is a little quirky and there's no way to query for future values after you set them so I had to make a pretty significant wrapper called `envelope.js`

On the Elm side, when you click a chord play button, I generate a list of `AudioChange` objects, such as `AddNote { t : Float, f : Float }` or `MuteAllNotes`. Then I encode them with a custom json encoder and send them through a port to JS. So the entire arpeggio is sent to the Web Audio API as soon as you click the button.

To update the UI when the arpeggio finishes playing, I use the `AnimationFrame` package to query the audio clock continuously and update the UI when a time threshold is passed. I had to write a native module to access the audio clock.

Possible brackets:
```
---+ +- =
---+ #===
---# #===
- =# #===
- =# #= -

-+ +---#
-+ +- =#
-+ #===#
-# #= -+

+---+ #=
+---# #=
+- =# #=
#===# +-
```

Chord flavors:
```
M
m
o
+
sus4
sus2
7
m7 / 6
0 / m6
M7
o7
mM7

9 = 7 + 0
M9 = M7 + m7
add9
madd9
m9 = m7 + M7
7b9 = 7 + o7
```
