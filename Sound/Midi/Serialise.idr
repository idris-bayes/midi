module Sound.Midi.Serialise

import Data.Bits
import Data.Fin
import Data.List
import Data.List1

import Sound.Midi.Types


--- HELPERS

||| Convert an integer into a little-endian encoded List of ints.
intToLE : Int -> List Int
intToLE = go []
  where go : List Int -> Int -> List Int
        go l n = if n > 0xFF
                    then go ((n `mod` 256) :: l) (n `div` 256)
                    else n :: l

||| Convert an integer into a variable length quantity.
intToVLQ : Int -> List Int
intToVLQ n = if n < 128 then [n] else go [n `mod` 128] $ n `div` 128
  where go : List Int -> Int -> List Int
        go l n = if n > 0x7F
                    then go ((128 + n `mod` 128) :: l) (n `div` 128)
                    else 128 + n :: l

||| Pad the beginning of a list with n zeroes.
padList : Nat -> List Int -> List Int
padList n l = case length l `compare` n of
  LT => (replicate (n `minus` length l) 0) ++ l
  EQ => l
  GT => ?padList_too_large

||| Serialise an int with a given padding length.
serInt : Nat -> Int -> List Int
serInt l = padList l . intToLE

||| Serialise a length value.
serLen : Int -> List Int
serLen = serInt 4

serString : String -> List Int
serString s = intToVLQ (cast $ length s + 1) ++ (map cast $ fastUnpack s) ++ [0]


--- SERIALISATION

||| Serialise a Meta Event.
serMetaEvent : ME -> List Int
serMetaEvent (SequenceNr n) = ?unimpl_serMetaEvent_sequencerNr
serMetaEvent (TextEvent      s) = 0x01 :: serString s
serMetaEvent (Copyright      s) = 0x02 :: serString s
serMetaEvent (SequenceName   s) = 0x03 :: serString s
serMetaEvent (InstrumentName s) = 0x04 :: serString s
serMetaEvent (Lyric          s) = 0x05 :: serString s
serMetaEvent (Marker         s) = 0x06 :: serString s
serMetaEvent (CuePoint       s) = 0x07 :: serString s
serMetaEvent (ProgramName    s) = 0x08 :: serString s
serMetaEvent (DeviceName     s) = 0x09 :: serString s
serMetaEvent (ChannelPrefix c) = [0x20, 0x01, cast $ finToNat c]
serMetaEvent (MidiPort n) = [0x21, 0x01, n]
serMetaEvent EndOfTrack = [0x2F, 0x00]
serMetaEvent (SetTempo n) = [0x51, 0x03] ++ serInt 3 n  -- TODO: SMPTE support
serMetaEvent (SMPTEOffset x) = ?unimpl_serMetaEvent_SMPTE
serMetaEvent (TimeSig n d c b) = [0x58, 0x04, n, d, c, b]
serMetaEvent (KeySig a m) = [0x59, 0x02, a, if m then 1 else 0]
serMetaEvent (SequencerME l xs) = ?unimpl_serMetaEvent_SequencerME

||| Serialise a MIDI Channel Mode event
serChMode : ChModeMsg -> List Int
serChMode AllSoundOff        = [120, 0x00]
serChMode (ResetAllCtrlrs x) = [121, x]
serChMode (LocalCtrl x)      = [122, if x then 127 else 0]
serChMode AllNotesOff        = [123, 0x00]
serChMode OmniOff            = [124, 0x00]
serChMode OmniOn             = [125, 0x00]
serChMode (MonoOn x)         = [126, x]
serChMode PolyOn             = [127, 0x00]

||| Serialise a MIDI Channel Voice event.
serMidiEvent : ChVoice -> List1 Int
serMidiEvent (NoteOff    p v) = 0x80 ::: [p, v]
serMidiEvent (NoteOn     p v) = 0x90 ::: [p, v]
serMidiEvent (Aftertouch n v) = 0xA0 ::: [n, v]
serMidiEvent (CtrlChange c v) = 0xB0 ::: [c, v]
serMidiEvent (ChMode x) = 0xB0 ::: serChMode x
serMidiEvent (ProgChange p)   = 0xC0 ::: [p]
serMidiEvent (ChPressure p)   = 0xD0 ::: [p]
serMidiEvent (PitchBend b)    = 0xE0 ::: [b]

||| Serialise a MIDI event.
serEvent : Event -> List Int
serEvent (MetaEvt e) = 0xFF :: serMetaEvent e
serEvent (MidiEvt (MkChMsg c v)) = let (x:::xs) = serMidiEvent v
                                    in (cast (finToNat c) .|. x) :: xs
serEvent (SysExEvt e) = ?unimpl_serEvent_sysEx

||| Serialise a track event (event paired with time difference).
serTrkEvent : TrkEvent -> List Int
serTrkEvent (TE dt e) = intToVLQ dt ++ serEvent e

||| Serialise a MIDI chunk, be it a header or a track.
serChunk : Chunk -> List Int
serChunk (Header _ f t c) = [0x4d, 0x54, 0x68, 0x64, 0, 0, 0, 0x06]  -- Magic bytes
                         ++ [0, cast $ finToNat f]  -- Format code
                         ++ serInt 2 t
                         ++ serInt 2 c
serChunk (Track _ es)     = let es' = join $ map serTrkEvent es in
                            [0x4d, 0x54, 0x72, 0x6b]  -- Magic bytes
                         ++ serLen (cast $ length es')
                         ++ es'

||| Serialise a MIDI file representation.
export
serialise : MidiFile -> List Int
serialise = join . map serChunk
