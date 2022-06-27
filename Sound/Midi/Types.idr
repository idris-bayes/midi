module Sound.Midi.Types

import Data.Fin
import Data.List
import Data.List1
import Data.Vect

-- Largely implemented with reference to https://www.midi.org/specifications-old/item/table-1-summary-of-midi-message
-- also https://www.cs.cmu.edu/~music/cmsip/readings/Standard-MIDI-file-format-updated.pdf

||| Specifies a MIDI Channel (1-16)
public export
Channel : Type
Channel = Fin 16
-- TODO: Change all of these to be bounded properly
||| Specifies a note value.
public export
Note : Type
Note = Int8  -- 0 to 127

||| Specifies a general value in [0, 127].
public export
Value : Type
Value = Int8  -- 0 to 127
||| Used when values in [0, 16384] are needed.
public export
DValue : Type
DValue = Int16  -- 0 to 16384

||| Specifies a controller number for control changes.
public export
Controller : Type
Controller = Int8  -- 0 to 119

||| Specifies a program/patch number.
public export
Program : Type
Program = Int8  -- 0 to 127?

||| Specifies a Time Code message type.
public export
TimeCodeMsg : Type
TimeCodeMsg = Fin 8
||| Value to be used in a Time Code message.
public export
TimeCodeVal : Type
TimeCodeVal = Fin 16

||| Channel Mode messages.
public export
data ChModeMsg = ||| Instructs all oscillators to turn off immediately, setting
                 ||| VCAs to zero, regardless if the hold pedal is enabled.
                 AllSoundOff  -- c=120, v=0
               | ||| Sets all controllers to their default values. Value must be 0,
                 ||| unless otherwise defined by a manufacturer.
                 ResetAllCtrlrs Value -- c=121, v=x, usually 0
               | ||| If False, all devices on the channel will respond only to MIDI
                 ||| input. If True, local controllers will also be accepted.
                 LocalCtrl Bool  -- c=122, v=0 or 127
               | ||| Disables all oscillators. If the hold pedal is enabled, the
                 ||| note will sound until it is released.
                 AllNotesOff  -- c=123, v=0
               -- The following four messages also cause AllNotesOff
               | ||| Disables Omni mode. Also sends AllNotesOff.
                 OmniOff  -- c=124, v=0
               | ||| Enables Omni mode. Also sends AllNotesOff.
                 OmniOn  -- c=125, v=0
               | ||| Enables Mono mode, disabling Poly mode. The given Value should
                 ||| be the number of channels if Omni mode is disabled, or 0 if it
                 ||| is enabled.
                 MonoOn Value  -- c=126, v=number of channels if omni off, or 0 if omni on
               | ||| Enables Poly mode, disabling Mono mode.
                 PolyOn  -- c=127, v=0

||| Channel Voice messages.
public export
data ChVoice = NoteOff Note Value
             | NoteOn  Note Value
             | ||| Polyphonic Aftertouch. Sets a "pressure" value for a given
               ||| note. This is often sent by pressing down harder on a key
               ||| after it "bottoms out".
               Aftertouch Note Value
             | ||| Control Change (CC). Sent when a controller changes (e.g. mod
               ||| wheel, foot pedal, etc.). Value may range up to 119, values
               ||| 120-127 are handled in ChMode.
               CtrlChange Controller
             | ||| Reserved CC values.
               ChMode ChModeMsg
             | ||| Program Change. This message is sent when the patch number
               ||| changes.
               ProgChange Program
             | ||| Channel Pressure. Differs from Polyphonic Aftertouch in that
               ||| it only sends the single greatest pressure value of all
               ||| depressed keys.
               ChPressure Value
             | ||| Pitch Bend change. Value is centred around 0x2000.
               PitchBend DValue

||| Channel messages.
public export
data ChMsg = MkChMsg Channel ChVoice

||| SysEx manufacturer codes.
public export
data Manufacturer = ||| Universal Exclusive Message (UEM) code. This is not
                    ||| manufacturer specific, and will be recognised by any
                    ||| device that may support the message.
                    Universal Value
                  | ||| Specific manufacturer code. Consists of 3x7bit values,
                    ||| and is only recognised by devices registered under that
                    ||| manufacturer.
                    Specific (Vect 3 Value)

||| System Common messages.
public export
data SysComm = ||| System Exclusive messages (SysEx). Due to being manufacturer
               ||| specific, no direct interface is provided. Instead, a list of
               ||| values may be sent to the device. Do not send the leading or
               ||| trailing bytes (0xf0 and 0xf7 respectively).
               SysEx Manufacturer (List Value)
             | ||| Time Code Quarter Frame. Sends one of 8 message types, with a
               ||| 4 bit value.
               TimeCodeQF TimeCodeMsg TimeCodeVal
             | ||| Sets the Song Position Pointer to the given value.
               SongPositionPtr DValue
             | ||| Selects a specified song.
               SongSelect Value
             | ||| Instructs all analogue oscillators to self-tune.
               TuneRequest
             | ||| Signals the end of a SysEx message.
               ExclEnd  -- TODO: this probably doesn't need to be exposed.

||| MIDI real-time system messages. These can be sent at any time, including
||| during SysEx messages.
public export
data SysRT = ||| Timing Clock. Sent 24 times per quarter note (crotchet) when
             ||| synchronisation is requested.
             TimingClk
           | ||| Starts the current sequence. This message must be followed with
             ||| Timing Clocks.
             StartSeq
           | ||| Continues a stopped sequence from where it left off.
             ContinueSeq
           | ||| Stops a playing sequence.
             StopSeq
           | ||| This messages is optional. It is intended to be used when
             ActiveSensing
           | Reset

public export
data SysMsg = Gdsgf

--------------------
-- File Type defs --

||| Tempo of a given track
public export
Tempo : Type
Tempo = Int

public export
SMPTE : Type
SMPTE = ?unimpl

public export
data ME = ||| Specifies the "number" of a sequence. If omitted, this is set to
          ||| the order in which the sequences occur.
          ||| Optional. Must occur at the beginning of a track.
          SequenceNr Int
        | ||| Allows arbitrary text embeds in the file. Holds no semantic meaning.
          TextEvent String
        | ||| Copyright string.
          Copyright String
        | ||| The name of a sequence (if in format 0 or track 1 of format 1), or
          ||| a track.
          SequenceName String
        | ||| The name of the instrument used in the track.
          InstrumentName String
        | ||| A lyric attached to a given event time. Generally used for each
          ||| syllable individually.
          Lyric String
        | ||| Normally only in track 1. Specifies a text "marker" in the score,
          ||| e.g. "First verse".
          Marker String
        | ||| Specifies an event in a film or stage show at the equivalent point
          ||| in the score, e.g. "curtain opens".
          CuePoint String
        | ||| Sends all following SysEx and meta events to the given channel,
          ||| until a MidiEvt is received with channel info or another Channel
          ||| Prefix is sent.
          ChannelPrefix Channel
        | ||| Specifies the end of the track.
          EndOfTrack
        | ||| Indicates a tempo change, in microseconds per MIDI quarter note.
          SetTempo Int
        | ||| Optional. If present, indicates at which SMPTE time code the track
          ||| is to start. Should be placed before any transmittable MIDI codes.
          SMPTEOffset SMPTE
        | ||| Sets a new time signature. The first two arguments are the
          ||| numerator and denominator, the third is the number of MIDI clocks
          ||| in a metronome tick, and the fourth is the number of notated 32nd
          ||| notes in a MIDI quarter note (24 MIDI clocks).
          TimeSig Int Int Int Int
        | ||| Sets a new key signature. The first value indicates the number of
          ||| accidentals (where negative values represent flats and positives
          ||| sharps). If the second is True, the keysig is minor, otherwise
          ||| major.
          KeySig Int Bool
        | ||| Sequencer Specific Meta Event. Similar to SysEx, this is
          ||| manufacturer-specific and represented simply as a list of values.
          SequencerME Manufacturer (List Value)

||| A given track event.
public export
data Event = ||| "Meta" event. Describes changes to the MIDI/track environment.
             MetaEvt ME
           | ||| Channel message. Encodes actual notes
             MidiEvt ChMsg
           | ||| SysEx message.
             SysExEvt SysMsg

||| A track event represents an action occuring at a specific time in a MIDI file.
public export
data TrkEvent = TE Int Event

||| Representation of MIDI chunks.
public export
data Chunk = ||| MIDI header chunk. Included
             Header Int (Fin 3) Int Int
           | Track Int (List TrkEvent)  -- TODO: List1

||| Maps an amount of accidentals to a human-readable key signature notation.
keySigAccs : Vect ? String
keySigAccs = ["F#/Gb", "Db", "Ab", "Eb", "Bb", "F",
              "C",
              "G",     "D",  "A",  "E",  "B"]

||| Given a number of accidentals, and a boolean (True if minor scale, False if
||| major), generate a human-readable representation of the key signature.
mkKeySig : Int -> Bool -> String
mkKeySig a m = index idx keySigAccs  -- TODO: handle minor scales
  where idx : Fin 12
        idx = restrict 11 (cast a + if m then 9 else 6)

public export
Show ME where
  show (SequenceNr i)       = "Sequence Number: " ++ show i
  show (TextEvent str)      = "Text Event: " ++ str
  show (Copyright str)      = "Copyright: " ++ str
  show (SequenceName str)   = "Sequence/Track name: " ++ str
  show (InstrumentName str) = "Instrument name: " ++ str
  show (Lyric str)          = "Lyric: " ++ str
  show (Marker str)         = "Marker: " ++ str
  show (CuePoint str)       = "Cue Point: " ++ str
  show (ChannelPrefix x)    = "Channel Prefix: " ++ show x
  show EndOfTrack           = "End of track."
  show (SetTempo i)         = "Set Tempo: " ++ show i
  show (SMPTEOffset x)      = "SMPTE Offset: " ++ ?x_unimpl
  show (TimeSig n d c q)    = "Time Signature: " ++ show n ++ "/" ++ show d
                           ++ ", " ++ show c ++ " clks/metronome tick, "
                           ++ show q ++ " 32nd notes/crotchet"
  show (KeySig a m)         = "Key Signature: " ++ mkKeySig a m ++ if m then " Minor"
                                                                  else " Major"
  show (SequencerME x is)   = "Sequencer Meta Event: cannot show"  -- TODO: byte list?

public export
Show ChModeMsg where
  show AllSoundOff        = "All sound off"
  show (ResetAllCtrlrs x) = "Reset all controllers"
  show (LocalCtrl b)      = (if b then "En" else "Dis") ++ "abled local control"
  show AllNotesOff        = "All notes off"
  show OmniOff            = "Disable Omni mode"
  show OmniOn             = "Enable Omni mode"
  show (MonoOn c)         = "Enabling Mono mode for " ++ show c ++ " channels"
  show PolyOn             = "Enabling Poly mode"

public export
Show ChVoice where
  show (NoteOff n v)    = "Note Off: "    ++ show n ++ ", " ++ show v
  show (NoteOn  n v)    = "Note On: "     ++ show n ++ ", " ++ show v
  show (Aftertouch n v) = "Aftertouch: "  ++ show n ++ ", " ++ show v
  show (CtrlChange c)   = "Control Change: Controller " ++ show c
  show (ChMode x)       = "Channel Mode: " ++ show x
  show (ProgChange p)   = "Program change to " ++ show p
  show (ChPressure x)   = "Channel Pressure: " ++ show x
  show (PitchBend b)    = "Pitch Bend: " ++ show b

public export
Show Event where
  show (MetaEvt  e) = "MetaEvt: " ++ show e
  show (MidiEvt (MkChMsg c m)) = "MidiEvt (channel " ++ show c ++ "): " ++ show m
  show (SysExEvt e) = "SysExEvt: cannot show"  -- TODO: byte list?

public export
Show TrkEvent where
  show (TE dt e) = "\n" ++ show dt ++ ": " ++ show e

public export
Show Chunk where
  show (Header len fmt tracks ticks) = "MThd (" ++ show len ++ " bytes): "
                                    ++ "Format " ++ show (finToNat fmt) ++ ", "
                                    ++ show tracks ++ " track(s), "
                                    ++ show ticks ++ " ticks per quarter note"
  show (Track len es) = "MTrk (" ++ show len ++ " bytes): "
                     ++ show es
