# Chords
Text-based chord progression editor

Type chord names in a textbox to create a sequence of play buttons that you
can use like an instrument.

Try it in your browser: http://evanshort.name/chords/

## Current features
- Chords in textbox are colored to match play buttons
- Play chords as arpeggio, strum pattern, continuous pad, or basic strum
- Queue up a second chord to play in the next measure
- Select a key signature to transpose the entire chord progression
- Save progressions in URL to bookmark or share them
- Adjustable tempo
- Playable circle of 5ths
- 7th, 9th, and 13th chords
- Table showing all chords in key
- View recently played chords to capture improvised sequences
- On-screen piano keyboard that displays the most recently played chord
- Search for chords by selecting piano keys
- Option to save settings in local storage
- Tutorial for new users

## Planned features
- Option to input chords as roman numerals
- Tap to set tempo
- Syntax to set duration of each chord
- Chord duration saved in "Recently played" view
- Export progression as MIDI
- Buttons to play entire line of progression
- View different Greek modes in chord table
- 7th chords displayed within circle of 5ths, with show/hide option

## Possible features
- More arpeggio styles
- Mobile-friendly interface
- Suggestions for next chord in progression
- Support for selected slash chords
- Show chords on musical staff
- Show guitar chord diagrams
- Somehow indicate which notes are in the current key signature
- Allow focusing chords using arrow keys

## Color choices
The chord colors basically follow this scheme:
- blue = tonic
- red = dominant
- green = subdominant

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

## Build steps
First, download Elm from http://elm-lang.org/

To build in Atom, install the
[Atom Build package](https://atom.io/packages/build) and press cmd+alt+b /
ctrl+alt+b to execute the build command found in
[`.atom-build.yml`](https://github.com/evanshort73/chords/blob/master/.atom-build.yml).

To build from the command line, navigate to the repo folder and enter the
build command found in
[`.atom-build.yml`](https://github.com/evanshort73/chords/blob/master/.atom-build.yml).

To run, open `index.html` in your browser.

In Microsoft Edge, accessing local storage from a page that is loaded from
your filesystem causes an internal error. To test on Microsoft Edge, edit
[`index.html`](https://github.com/evanshort73/chords/blob/master/index.html)
and set `canStore` to `false`.

## Implementation details
On the JS side, I use the Web Audio API to create a new chain of audio nodes
for each note that is played. A sawtooth oscillator feeds into a gain node
and then a lowpass filter, both of which decrease over time. There is a loop
that disconnects the filter nodes so they can be garbage-collected.

On the Elm side, when you click a chord play button, I generate a list of
`AudioChange` objects, such as `AddNote { t : Float, f : Float }` or
`MuteAllNotes`. Then I encode them with a custom JSON encoder and send them
through a port to JS. So the entire arpeggio is sent to the Web Audio API as
soon as you click the button. In order for Elm to schedule the notes at the
right time, I had to write a native module called
[`AudioTime`](https://github.com/evanshort73/chords/blob/master/src/AudioTime.elm)
to access the audio clock.

Elm also sets "alarms", which are times when the UI needs to be updated. The
loop in [`audio.js`](https://github.com/evanshort73/chords/blob/master/audio.js)
notifies Elm when an alarm has passed.

In order to programmatically replace text in the textarea without erasing the
undo history, we secretly create a new textarea for every replacement. If the
textarea receives a ctrl+z key press and the text doesn't change within 5ms,
we assume that the user is trying to undo the programmatic change and we
restore the old textarea. Same goes for redo. In Chrome and Edge, pressing
ctrl+z can also affect the hidden textareas so we have to detect and
counteract that. This is all handled on the JS side in
[`theater.js`](https://github.com/evanshort73/chords/blob/master/theater.js),
with an Elm interface in
[`Theater.elm`](https://github.com/evanshort73/chords/blob/master/src/Theater.elm).

## Design principles

### Keep it flat flat flat
Refactor code when it becomes hard to manage, not when it becomes repetitive.
Refactoring for the sake of code re-use tends to introduce extra abstraction
layers which can hide opportunities for more meaningful refactors. The same
is true of refactors that attempt to increase encapsulation.

### Simple is better than pure
It's usually a bad idea to write extra code that smooths over an ugly hack for
the sake of "correctness". Expect new features to introduce rough edges to the
code base. If you let ugly code sit for a while, you'll often find a better
solution while working on a different feature.

### Always be open to removing features
The most important refactors start by questioning the utility of a feature you
always thought was essential. This project was essentially dead for two months
because the code got too complicated to maintain. I was able to bring it back
by removing the ability to invert chords, a feature that had been baked into
the design from the beginning.

### Accept no glitches
As users, we don't always notice how much mental energy we spend compensating
for interfaces that are slightly unpredictable. We develop a lot of
unconscious behaviors like double-checking that a certain element has focus.
This program is meant to act like an extension of the user's brain as they
reason about chords, so it's very important to fix any glitches that might sap
their mental energy.

### The UI should be normal
Use standard controls like checkboxes, buttons, and drop-down menus. Don't
style them with custom CSS. Yes, I know broke this rule by making fancy
Mac-inspired radio buttons. Each UI element should have an explicit text
description that's always visible so users know what it does at a glance.
Don't hide things to save space unless absolutely necessary.

### Strong opinions loosely held
Keep the UI simple by not overwhelming the user with choices. This means
having an opinion about everything. If a user wants something to be
customizable, see if you can accommodate their use case by changing your
opinion instead.

### Variable names
Prefer single-word variable names that refer to concrete objects in the real
world. Use a thesaurus to spark your creativity but forego the long words.
Take advantage of metaphors to name multiple related variables. For example, a
1D list containing multiple rows of data separated with a delimiter is called
a `train`, and each row of data is called a `car`.

### Fail loudly
It's not always practical to rely on Elm's guarantee of no runtime errors. For
example, you might let `y = x % 12` and then assume that `y < 12`. Or you
add an item to a list and then assume that the list is not empty. Make these
assumptions explicit by calling `Debug.crash` if they fail and printing some
useful information.

### Code style
I don't have strong opinions about code style and formatting; I'm just doing
what works for me. I'd like to start using elm-format but I'm afraid it will
ruin the commit history.
