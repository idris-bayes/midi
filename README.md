# midi
A MIDI library for Idris2.

## Installation
Add this repo to your `pack.toml` and then add to your project's `.ipkg`.

## Completeness
We currently implement parsing for almost all of the MIDI 1.1 file specification.
The main exceptions are the non-reserved control changes, and universal SysEx messages.
SysEx and Sequencer Meta Events are intentionally unimplemented,
as they are device specific.

## To-do
- Implement SysEx, SequencerME and SMPTE commands
- The serialiser is very slow, speed could be increased by using difference lists or switching to a WriterT or something
- The serialiser doesn't use running status, leading to larger than neccessary files.
