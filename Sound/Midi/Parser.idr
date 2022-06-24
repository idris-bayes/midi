module Sound.Midi.Parser

import Data.Bits
import Data.List1
import Data.String
import Data.String.Parser
import Data.Vect

import Sound.Midi.Types

import Data.Buffer
import System.File

MidiFile : Type
MidiFile = List Chunk

getVal : Parser Int
getVal = map cast $ satisfy $ const True

take : (n : Nat) -> Parser (Vect n Int)
take n = ntimes n getVal

getString : (n : Nat) -> Parser String
getString n = do
  v <- take n
  ?gs_bdy

||| Parses a little-endian n-bit byte.
parseInt : (n : Nat) -> Parser Int
parseInt n = (foldl (\a, e => 256 * a + e) 0) <$> take n

--- MIDI-specific parsers
||| Parses a variable-length encoded value.
parseVLE : Parser Int
parseVLE = do
  bs <- many $ satisfy (> 'O')  -- 0x79, saves a cast
  let bsum = foldl (\a, c => 128 * a + (the Int $ cast c) - 0x80) 0 bs
  pure $ bsum * 128 + !getVal

||| Parses an SMPTE time code.
parseSMPTE : Parser SMPTE
parseSMPTE = ?smpte_parse

mutual
  ||| Parses a MIDI file format number.
  format : Parser (Fin 3)
  format = case integerToFin (cast !(parseInt 2)) 3 of
    Nothing  => fail "Invalid MIDI format."
    Just fin => pure fin

  ||| Parses a MIDI file header.
  header : Parser Chunk
  header = do
    skip $ string "MThd"
    len <- parseInt 4  -- TODO: should always be 6, maybe check for this?

    fmt <- format
    tracks <- parseInt 2
    ticks <- parseInt 2

    pure $ Header len fmt tracks ticks

  ||| Parses a Sequence Number Meta Event.
  sequenceNrME : Parser ME
  sequenceNrME = ?snme

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
    if (meType .&. 0xF0) == 0 then textME meType else
      case meType of
        0x00 => sequenceNrME
        0x20 => pure $ ChannelPrefix $ restrict 15 $ cast !getVal
        0x2F => pure EndOfTrack
        0x51 => pure $ SetTempo !(parseInt 3)
        0x54 => pure $ SMPTEOffset ?parse_smpte
        0x58 => pure $ TimeSig !(getVal) (cast $ pow 2 $ cast!(getVal)) !(getVal) !(getVal)
        0x59 => pure $ KeySig !(getVal) $ !(getVal) > 0
        0x7F => pure $ SequencerME (Universal 0) []  -- TODO: impl
        e     => fail $ "Invalid Meta Event type: " ++ show e

  ||| Parses an event
  event : Parser Event
  event = do
    pure $ MetaEvt !metaEvent

  ||| Parses an event at a timecode in a track.
  trackEvent : Parser TrkEvent
  trackEvent = do
    dt <- parseVLE
    e <- event
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
    pure $ hdr :: trks


testHeader = "MThd\x0\x0\x0\x6\x0\x1\x0\x1\x1\xe0MTrk\x0\x0\x0\x6e\x0\xff\x03\x06\x50\x69"


parseFile : String -> IO ()
parseFile filename = do
  bufE <- createBufferFromFile filename
  case bufE of
    Left e => print e
    Right buf => do
      size <- rawSize buf
      s <- getString buf 0 size
      case (parse file s) of
        Left e => print e
        Right (v,l) => --print "Read " ++ show l ++ printLn " bytes of input:" ++ printLn v
          printLn v
