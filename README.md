# Goodboy
A pure OCaml Gameboy emulator

This project is a work in progress Gameboy emulator written in pure OCaml.
The goal is to provide a full Gameboy + Gameboy Color emulator with support for most cartridges kind out there, and experiment with a few different frontends.

## Current progress

Currently most of the core logic is implemented, minus a few bugs, and simple games can be played. (Tetris and Tobu Tobu Girl are good examples)
I'm currently working on fixing more core logic bugs causing some games to fail and will then proceed to more complicated games.

Currently implemented:
- Gameboy CPU (cpu_instrs running successfully minus test 02)
- Timers
- GPU (Window, Background, Sprites)
- MBC1 and MBC0 type cartridges
- SDL and Notty frontends

On my todo list:
- Audio processing unit
- GBC and SGB modes
- Javascript frontend

The codebase is as of now a bit of a mess, everything is slightly more complicated than it needs to be and the overall performances are not optimal.
The interfaces will change dramatically in the future, I have never written an emulator, and the patterns to use were slightly blurry to me.
