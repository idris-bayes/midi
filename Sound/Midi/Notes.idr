module Sound.Midi.Notes

import Data.Fin

||| A Pitch is an integer, ranging from 0 to 127, with middle C as 60.
Pitch : Type
Pitch = Int

||| A Duration represents the amount of time a note is played for, where a value
||| of 1 represents one quarter note/crotchet.
Duration : Type
Duration = Double

||| A Note is a Pitch paired with a Duration.
Note : Type
Note = (Pitch, Duration)

||| A Chord is a set of notes.
Chord : Type
Chord = List Note

c4 : Note
c4 = (60, 1)
