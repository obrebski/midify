# midify

- converting X to MIDI where X includes Euterpea
    
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

Opening MIDI stream sending MIDI data to MIDI device 2.

    ```Haskell
    ghci> s <- start 2
    ```

## Write MIDI messages to stream `s`

    ```Haskell
    ghci> write s $ send (NoteOn 0 60 100)
    ghci> write s $ send (NoteOff 0 60 100)
    ```
