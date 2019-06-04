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

## Screenshots

Debugger
![Notty debugger interface](https://user-images.githubusercontent.com/146049/58849095-fa718880-86c3-11e9-973a-c8287f1a7d3f.png)

Notty frontend (running Tetris)
![Notty frontend](https://user-images.githubusercontent.com/146049/58849093-f8a7c500-86c3-11e9-8c6b-3ff5622f5d92.png)

SDL frontend (running Tobu Tobu Girl)
![SDL frontend](https://user-images.githubusercontent.com/146049/58849088-f47ba780-86c3-11e9-94da-cf8d07729544.png)
