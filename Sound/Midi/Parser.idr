||| Parser for MIDI 1.1 files.
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

MidiFile : Type
MidiFile = List Chunk

||| Gets a single value from the input.
getVal : Parser Int
getVal = (map cast $ satisfy $ const True)
     <?> "unexpected end of string in getVal"

||| Gets the next `n` values from the input as a vector.
take : (n : Nat) -> Parser (Vect n Int)
take n = ntimes n getVal
     <?> "unexpected end of string while trying to take \{show n} bytes"

||| Get `n` characters from the input and parses as a string.
getString : (n : Nat) -> Parser String
getString n = (pure $ fastPack $ map cast $ toList !(take n))
          <?> "couldn't read as string"

||| Parses a little-endian n-bit byte.
parseInt : (n : Nat) -> Parser Int
parseInt n = (foldl (\a, e => 256 * a + e) 0) <$> take n

--- MIDI-specific parsers
||| Parses a variable-length encoded value.
parseVLE : Parser Int
parseVLE = do
  bs <- many $ satisfy (> 0x7F)
  let bsum = foldl (\a, c => 128 * a + (the Int $ cast c) - 0x80) 0 bs
  pure $ bsum * 128 + !(getVal <?> "couldn't parse VLE literal")

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

  ||| Parses a Sequence Number Meta Event. Takes length as an argument;
  ||| if l == 0x00 then use default values
  ||| if l == 0x02 then parse and use supplied value
  ||| otherwise, fail
  sequenceNrME : Int -> Parser ME
  sequenceNrME l = ?snme

  ||| Parses a text-based Meta Event
  textME : Int -> Parser ME
  textME t = do
    len <- parseInt 4
    str <- getString $ cast len
    case t of
      0x01 => pure $ TextEvent str
      0x02 => pure $ Copyright str
      0x03 => pure $ SequenceName str
      0x04 => pure $ InstrumentName str
      0x05 => pure $ Lyric str
      0x06 => pure $ Marker str
      0x07 => pure $ CuePoint str
      e     => fail $ "Invalid text Meta Event type: " ++ show e

  ||| Parses a MIDI Meta Event.
  metaEvent : Parser ME
  metaEvent = do
    meType <- getVal
    meLen <- getVal
    if (meType .&. 0xF0) == 0 then textME meType else
      case (meType, meLen) of
        (0x20, 0x01) => pure $ ChannelPrefix $ restrict 15 $ cast !getVal
        (0x2F, 0x00) => pure EndOfTrack
        (0x51, 0x03) => pure $ SetTempo !(parseInt 3)
        (0x54, 0x05) => pure $ SMPTEOffset ?parse_smpte
        (0x58, 0x04) => pure $ TimeSig !(getVal) (cast $ pow 2 $ cast!(getVal)) !(getVal) !(getVal)
        (0x59, 0x02) => pure $ KeySig !(getVal) $ !(getVal) > 0
        (0x7F, l)    => pure $ SequencerME (Universal 0) []  -- TODO: impl
        (t,    l)    => fail $ "Invalid Meta Event type: " ++ show t ++ " with length " ++ show l

  ||| Parses an event
  event : Parser Event
  event = do
    eType <- getVal
    case eType of
      0xFF => pure $ MetaEvt !metaEvent
      e    => fail "unexpected event type: \{show e}"

  ||| Parses an event at a timecode in a track.
  trackEvent : Parser TrkEvent
  trackEvent = do
    dt <- parseVLE
    e <- event <?+> "failed reading event at timecode \{show dt}"
    pure $ TE dt e

  ||| Parses a track chunk.
  track : Parser Chunk
  track = do
    skip $ string "MTrk"
    len <- parseInt 4
    es <- some trackEvent
    pure $ Track len $ singleton $ TE 0 $ MetaEvt EndOfTrack

  ||| Parses a full MIDI file.
  file : Parser MidiFile
  file = do
    hdr <- header
    trks <- some track
    skip $ eos
    pure $ hdr :: trks


testHeader = "MThd\x0\x0\x0\x6\x0\x1\x0\x1\x1\xe0MTrk\x0\x0\x0\x6e\x0\xff\x03\x06\x50\x69"


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
