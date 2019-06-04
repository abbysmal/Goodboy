module Bs = Bigstringaf

type t = {
  mutable in_bios : bool;
  bios : Addressable.t;
  vram : Addressable.t;
  wram0 : Addressable.t;
  wram1 : Addressable.t;
  cartridge : Cartridge.t;
  oam : Addressable.t;
  io_ports : Addressable.t;
  joypad : Joypad.t;
  hram : Addressable.t;
  mutable serial_out : Uint8.t option;
}

let bios           = 0x0000, 0x00FE
let cartridge      = 0x0000, 0x7FFF
let vram           = 0x8000, 0x9FFF
let cartridge_ram  = 0xA000, 0xBFFF
let wram0          = 0xC000, 0xCFFF
let wram1          = 0xD000, 0xDFFF
let echo1          = 0xE000, 0xEFFF
let echo2          = 0xF000, 0xFDFF
let oam            = 0xFE00, 0xFE9F
let unused         = 0xFEA0, 0xFEFF
let io_ports       = 0xFF00, 0xFF7F
let hram           = 0xFF80, 0xFFFF

let make ?boot ~rom =
  let in_bios, bios =
    match boot with
    | None -> false, Addressable.void ()
    | Some buffer -> true, (Addressable.make ~buffer bios)
  in
  let s = {
    bios;
    in_bios;
    vram = Addressable.make vram;
    wram0 = Addressable.make wram0;
    wram1 = Addressable.make wram1;
    oam = Addressable.make oam;
    cartridge = Cartridge.make ~rom;
    io_ports = Addressable.make io_ports;
    hram = Addressable.make hram;
    joypad = Joypad.make ();
    serial_out = None;
  } in
  Bs.set s.io_ports.buffer 0 (Uint8.chr 0xCF);
  s
