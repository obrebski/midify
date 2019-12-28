# midify

- converting X to [MIDI](midi.org) where X includes [Message](https://hackage.haskell.org/package/HCodecs-0.5.1/docs/Codec-Midi.html#t:Message) (Codec.Midi), [Music Pitch](http://hackage.haskell.org/package/Euterpea-1.1.1/docs/Euterpea-Music-Note-Music.html#t:Music) (Euterpea).


    
- writing X to PMStream ([Sound.PortMidi](https://hackage.haskell.org/package/PortMidi-0.2.0.0/docs/Sound-PortMidi.html))

# Installation

`midify` is a `stack` package. If you do not have `stack` installed, see [this page](https://docs.haskellstack.org/en/stable/install_and_upgrade/).


1. Clone the repository
    
    ```console
    $ git clone https://github.com/obrebski/midify.git
    ```
2. Go to `midify` directory
    
    ```console
    $ cd midify
    ```

3. Run `ghci`

    ```console
    $ stack ghci
    ```

Note: Sometimes it is necessary to run `stack setup` before `stack ghci`.

# Quick tutorial

## Checking for [MIDI](midi.org) devices available

Use `devices` from Euterpea module re-exported by Midify for conveniance:

```Haskell
ghci> devices

Input devices: 
  InputDeviceID 1       Midi Through Port-0

Output devices: 
  OutputDeviceID 0      Midi Through Port-0
  OutputDeviceID 2      TiMidity port 0
  OutputDeviceID 3      TiMidity port 1
  OutputDeviceID 4      TiMidity port 2
  OutputDeviceID 5      TiMidity port 3

```

## Opening [MIDI](midi.org) stream

Opening [MIDI](midi.org) stream to send [MIDI](midi.org) data to [MIDI](midi.org) device 2:

```Haskell
ghci> s <- start 2
```

## Writing [MIDI](midi.org) messages to a stream

Use `send` method to write [MIDI](midi.org) data to a stream.

```Haskell
ghci> write s $ send (NoteOn 0 60 100)
ghci> write s $ send (NoteOff 0 60 100)
```

Other [MIDI](midi.org)
[Message](https://hackage.haskell.org/package/HCodecs-0.5.1/docs/Codec-Midi.html#t:Message)s
([Codec.Midi](https://hackage.haskell.org/package/HCodecs-0.5.1/docs/Codec-Midi.html)).

## Writing more than one message at a time

`send X` is a monadic action so...

```Haskell
ghci> write s $ send (NoteOn 0 60 100) >> send (NoteOff 0 60 100)
```

The second message will be sent immediately after the first one. In
order to make the note last, time delay must be introduced.

## Time delay

To introduce time delay, use the action `pause`:

```Haskell
ghci> write s $ send (NoteOn 0 60 100) >> pause 1 >> send (NoteOff 0 60 100)
```

The time unit is whole note duration. The argument to `pause` is of
type `Rational`, the same as `Dur` type in Euterpea, so you can use
duration constants from Euterpea.

```Haskell
ghci> write s $ send (NoteOn 0 60 100) >> pause wn >> send (NoteOff 0 60 100)
ghci> write s $ send (NoteOn 0 60 100) >> pause (4*qn) >> send (NoteOff 0 60 100)
```

## Sending Euterpea code

`send` is an overloded function accepting instances of the
`Midifiable` class. `Music Pitch` is an instance of `Midifiable`, so:

```Haskell
ghci> write s $ send (c 4 qn) >> pause wn >> send (d 4 qn)
ghci> write s $ send (c 4 qn :+: rest wn :+: d 4 qn)
```
The two lines above generate the same MIDI output.


## MIDI shortcuts


## MIDI environment

The default values of channel (`ch`), NoteOn velocity (`vel`) and
NoteOff velocity (`vel'`) are kept in MIDI environment accessible
during evaluation of MIDI expressions. The initial values are 0 for
`ch` and 64 for `vel` and `vel'`.

MIDI channes are numbered from 0 to 15. Velocity values range from 0 to 127.

### Setting MIDI environment parameters

```Haskell
ghci> write s $ env[ch=3] >> env[vel=100] >> send (c 4 qn)
ghci> write s $ env[ch=3,vel=100] >> send (c 4 qn)
```

The note will be sent to channel 3 with NoteOn velocity 100 and NoteOff velocity 64.

