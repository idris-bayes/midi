||| Parser for MIDI 1.1 files.
||| Based on info from https://www.midi.org/specifications/file-format-specifications/standard-midi-files
module Sound.Midi.Parser

import Data.Bits
import Data.List1
import Data.String
import Data.Vect

import Data.Binary.Parser
import Sound.Midi.Types

import Data.Buffer
import System.File

--%default total

||| We parse MIDI files as vectors of integers
Parser : Type -> Type
Parser = Parser.Parser Int

MidiFile : Type
MidiFile = List Chunk

||| Get `n` characters from the input and parses as a string.
getString : (n : Nat) -> Parser String
getString n = (pure $ init $ fastPack $ map cast $ toList !(take n))
          <?> "couldn't read as string"
  where init : String -> String
        init s = strSubstr 0 (cast $ minus (length s) 1) s

||| Parses a little-endian n-bit byte.
parseInt : (n : Nat) -> Parser Int
parseInt n = (foldl (\a, e => 256 * a + e) 0) <$> take n

--- MIDI-specific parsers
||| Parses a variable-length encoded value.
parseVLE : Parser Int
parseVLE = do
  bs <- takeWhile (> 0x7F)
  let bsum = foldl (\a, c => 128 * a + c - 0x80) 0 bs
  pure $ bsum * 128 + !(anySingle <?> "couldn't parse VLE")

||| Parses an SMPTE time code.
parseSMPTE : Parser SMPTE
parseSMPTE = ?smpte_parse

mutual
  ||| Parses a MIDI file format number.
  format : Parser (Fin 3)
  format = do
    fmt <- parseInt 2
    case integerToFin (cast fmt) 3 of
      Nothing  => fail "invalid MIDI format \{show fmt}"
      Just fin => pure fin

  ||| Parses a MIDI file header.
  header : Parser Chunk
  header = do
    skip $ string "MThd" <?> "couldn't find MIDI header magic bytes"
    len <- parseInt 4

    if len /= 6  -- header length is always 6
      then fail "expected header length 6, got \{show len}"
      else do
        fmt <- format
        tracks <- parseInt 2
        ticks  <- parseInt 2
        pure $ Header len fmt tracks ticks

  ||| Parses a manufacturer ID.
  manufacturer : Parser Manufacturer
  manufacturer = do
    ?manu

  ||| Parses a System Exclusive message.
  sysEx : Parser SystemExclusive
  sysEx = do
    --m <- manufacturer
    len <- parseVLE
    let msg = toList $ the (Vect _ Value) $ map cast !(take $ cast len)
    end <- anySingle
    if end /= 0xF7
       then fail "expected 0xF7 (end of SysEx message) but got \{show end} after \{show len} bytes"
       else pure $ SE (Universal 0) msg  -- TODO: provide correct manufacturer

  ||| Parses a Sequence Number Meta Event. Takes length as an argument;
  ||| if l == 0x00 then use default values
  ||| if l == 0x02 then parse and use supplied value
  ||| otherwise, fail
  sequenceNrME : Int -> Parser ME
  sequenceNrME l = ?snme

  ||| Parses a text-based Meta Event
  textME : Int -> Int -> Parser ME
  textME t len = do
    str <- getString $ cast len
    case t of
      0x01 => pure $ TextEvent str
      0x02 => pure $ Copyright str
      0x03 => pure $ SequenceName $ str
      0x04 => pure $ InstrumentName str
      0x05 => pure $ Lyric str
      0x06 => pure $ Marker str
      0x07 => pure $ CuePoint str
      e     => fail $ "Invalid text Meta Event type: " ++ show e

  ||| Parses a MIDI Meta Event.
  metaEvent : Parser ME
  metaEvent = do
    meType <- anySingle
    meLen  <- anySingle
    if (meType .&. 0xF0) == 0 then textME meType meLen else
      case (meType, meLen) of
        (0x20, 0x01) => pure $ ChannelPrefix $ restrict 15 $ cast !anySingle
        (0x2F, 0x00) => pure EndOfTrack
        (0x51, 0x03) => pure $ SetTempo !(parseInt 3)
        (0x54, 0x05) => pure $ SMPTEOffset ?parse_smpte
        (0x58, 0x04) => pure $ TimeSig !(anySingle) (cast $ pow 2 $ cast!(anySingle)) !(anySingle) !(anySingle)
        (0x59, 0x02) => pure $ KeySig !(anySingle) $ !(anySingle) > 0
        (0x7F, l)    => pure $ SequencerME (Universal 0) []  -- TODO: impl
        (t,    l)    => fail $ "Invalid Meta Event type: " ++ show t ++ " with length " ++ show l

  ||| Parses a MIDI reserved control change message.
  chMode : Parser ChVoice
  chMode = do
    c <- anySingle
    v <- anySingle
    case c of
      _ => fail ""

  ||| Parses a generic control change message.
  ctrlChange : Parser ChVoice
  ctrlChange = pure $ CtrlChange (cast !(anySingle)) (cast !(anySingle))

  ||| Parses general MIDI events (such as notes).
  midiEvent : Int -> Parser ChMsg
  midiEvent evt = do
    let ch = restrict 15 $ cast $ evt .&. 0x0F
    case evt .&. 0xF0 of  -- TODO: bad
      0x80 => pure $ MkChMsg ch $ NoteOff    !(anySingle) !(anySingle)
      0x90 => pure $ MkChMsg ch $ NoteOn     !(anySingle) !(anySingle)
      0xA0 => pure $ MkChMsg ch $ Aftertouch !(anySingle) !(anySingle)
      0xB0 => pure $ MkChMsg ch $ !(chMode <|> ctrlChange)
      0xC0 => pure $ MkChMsg ch $ ProgChange !(anySingle)
      0xD0 => pure $ MkChMsg ch $ ChPressure !(anySingle)
      0xE0 => pure $ MkChMsg ch $ PitchBend  !(anySingle)
      _ => fail "invalid MIDI event code \{show evt}"

  ||| Parses an event
  event : Parser Event
  event = do
    eType <- anySingle
    case eType of
      0xF0 => pure $ SysExEvt !sysEx
      0xFF => pure $ MetaEvt !metaEvent
      e    => pure $ MidiEvt !(midiEvent e)

  ||| Parses an event at a timecode in a track.
  trackEvent : Parser TrkEvent
  trackEvent = do
    dt <- parseVLE <?> "couldn't parse timecode"
    e <- event <?+> "failed reading event at timecode \{show dt}"
    pure $ TE dt e

  ||| Parses a track chunk.
  track : Parser Chunk
  track = do
    skip $ string "MTrk"
    len <- parseInt 4
    es <- some trackEvent  -- todo: precalculate number of events to take?
    e <- trackEvent
    pure $ Track len es

  ||| Parses a full MIDI file.
  file : Parser MidiFile
  file = do
    hdr <- header
    trks <- some track
    pure $ hdr :: trks


parseFile : String -> IO ()
parseFile filename = do
  bufE <- createBufferFromFile filename
  case bufE of
    Left e => print e
    Right buf => do
      size <- rawSize buf

      l <- bufferData buf
      printLn l

      let v = fromList l
      case (parse file v) of
        Left e => putStrLn e
        Right (v,l) => --print "Read " ++ show l ++ printLn " bytes of input:" ++ printLn v
          printLn v
