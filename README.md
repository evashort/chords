# Chords
Text-based chord progression editor

Type chord names in a textbox to create a sequence of play buttons that you
can use like an instrument.

Try it in your browser: http://evanshort.name/chords/

## Current features
- Chords in textbox are colored to match play buttons
- Play chords as arpeggio, continuous pad, or basic strum
- Queue up a second chord to play in the next measure
- Select a key signature to transpose the entire chord progression
- Save progressions in URL to bookmark or share them
- Adjustable tempo
- Playable circle of 5ths
- 7th, 9th, and 13th chords
- Table showing all chords in key
- View recently played chords to capture improvised sequences

## Planned features
- Synth that changes timbre over time to keep notes from blending together
- More arpeggio styles
- Tempo-synced strum patterns
- Tap to set tempo
- Option to save playback settings in cookie
- View different Greek modes in chord table
- 7th chords displayed within circle of 5ths, with show/hide option
- Option to input chords as roman numerals
- On-screen piano keyboard that displays the most recently played chord
- Search for chords by selecting piano keys

## Color choices
The chord colors basically follow this scheme:
- blue = tonic
- red = dominant
- yellow/green = subdominant

Chords are arranged in rainbow order as you move up the major scale by thirds.
This way, triads that share two notes have neighboring colors.

Major and minor 7ths are colored by mixing the two triads they contain.

Diminished triads and chords that contain them are dark with white text.

The colors were chosen to be maximally saturated and close to the same
lightness in the
[CIELAB color space](https://en.wikipedia.org/wiki/CIELAB_color_space).
Since the red-green axis is missing for most people with color vision
deficiency, the lightness varies to compensate, with green being lightest and
red being darkest. The lightness was chosen to contrast with the text as
required by
[WCAG 2.0](https://www.w3.org/TR/UNDERSTANDING-WCAG20/visual-audio-contrast-contrast.html).

## Implementation details
On the JS side, I use the Web Audio API to set up 6 sawtooth oscillators,
connect each one to a gain node, and use Web Audio API commands like
`setTargetAtTime` to schedule frequency and amplitude changes in advance.
Unfortunately the API for scheduling future changes is a little quirky and
there's no way to query for future values after you set them so I had to make
a pretty significant wrapper called `envelope.js`

On the Elm side, when you click a chord play button, I generate a list of
`AudioChange` objects, such as `AddNote { t : Float, f : Float }` or
`MuteAllNotes`. Then I encode them with a custom json encoder and send them
through a port to JS. So the entire arpeggio is sent to the Web Audio API as
soon as you click the button.

To update the UI when the arpeggio finishes playing, I use the
`AnimationFrame` package to query the audio clock continuously and update the
UI when a time threshold is passed. I had to write a native module to access
the audio clock.

In order to programmatically replace text in the textarea without erasing the
undo history, we secretly create a new textarea for every replacement. If the
textarea receives a ctrl+z key press and the text doesn't change within 5ms,
we assume that the user is trying to undo the programmatic change and we
restore the old textarea. Same goes for redo. In Chrome and Edge, pressing
ctrl+z can also affect the hidden textareas so we have to detect and
counteract that. This is all handled on the JS side in `theater.js`, with an
Elm interface in `Theater.elm`.

# Design principles

## Keep it flat flat flat
Refactor code when it becomes hard to manage, not when it becomes repetitive.
Refactoring for the sake of code re-use tends to introduce extra abstraction
layers which can hide opportunities for more meaningful refactors. The same
is true of refactors that attempt to increase encapsulation.

## Always be open to removing features
The most important refactors start by questioning the utility of a feature you
always thought was essential. This project was essentially dead for two months
because the code got too complicated to maintain. I was able to bring it back
by removing the ability to invert chords, a feature that had been baked into
the design from the beginning.

## Fail loudly
It's not always practical to rely on Elm's guarantee of no runtime errors. For
example, you might let `y = x % 12` and then assume that `y < 12`. Or you
add an item to a list and then assume that the list is not empty. Make these
assumptions explicit by calling `Debug.crash` if they fail and printing some
useful information.

## Accept no glitches
As users, we don't always notice how much mental energy we spend compensating
for interfaces that are slightly unpredictable. We develop a lot of
unconscious behaviors like double-checking that a certain element has focus.
This program is meant to act like an extension of the user's brain as they
reason about chords, so it's very important to fix any glitches that might sap
their mental energy.

## The UI should be normal
Use standard controls like checkboxes, buttons, and drop-down menus. Don't
style them with custom CSS. Yes, I know broke this rule by making fancy
Mac-inspired radio buttons. Each UI element should have an explicit text
description that's always visible so users know what it does at a glance.
Don't hide things to save space unless absolutely necessary.

## Variable names
Prefer single-word variable names that refer to concrete objects in the real
world. Use a thesaurus to spark your creativity but forego the long words.
Take advantage of metaphors to name multiple related variables. For example, a
1D list containing multiple rows of data separated with a delimiter is called
a `train`, and each row of data is called a `car`.

## Code style
I don't have strong opinions about code style and formatting; I'm just doing
what works for me. I'd like to start using elm-format but I'm afraid it will
ruin the commit history.
