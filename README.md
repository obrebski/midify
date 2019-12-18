# midify

- converting X to MIDI where X includes Message (Codec.Midi), Music Pitch (Euterpea)
    
- writing X to PortMidi

# Installation

`midify` is a `stack` package. If you do not have `stack` installed, see [this](https://docs.haskellstack.org/en/stable/install_and_upgrade/).


1. Clone the repository
    
    ```console
    $ git clone https://github.com/obrebski/midify.git
    $ cd midify
    $ stack setup
    ```

# Tutorial

## Open MIDI stream

Opening MIDI stream to send MIDI data to MIDI device 2.

```Haskell
ghci> s <- start 2
```

## How to write MIDI messages to stream `s`

Use `send` method to write MIDI data to a stream.

```Haskell
ghci> write s $ send (NoteOn 0 60 100)
ghci> write s $ send (NoteOff 0 60 100)
```
## How to write more than one message at a time

```Haskell
ghci> write s $ send (NoteOn 0 60 100) >> send (NoteOff 0 60 100)
```

The second messages will be sent immediately after the first one. 

## How to force time delay

```Haskell
ghci> write s $ send (NoteOn 0 60 100) >> pause 1 >> send (NoteOff 0 60 100)
```

## How to send Euterpea code

```Haskell
ghci> write s $ send (c 4 qn) >> pause 1 >> send (d 4 qn)
ghci> write s $ send ((c 4 qn) :+: (d 4 qn))
```
