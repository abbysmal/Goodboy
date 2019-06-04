module Bs = Bigstringaf

module Constants = struct

let title = 0x134, 0x143
let manufacturer_code = 0x13F, 0x142
let cgb_flag = 0x143
let new_licensee_code = 0x144, 0x145
let sgb_flag = 0x146
let cartridge_flag = 0x147
let rom_size = 0x148
let ram_size = 0x149

end

let is_cgb rom =
  let f = Bs.get rom Constants.cgb_flag in
  match Uint8.code f with
  | 0x80
  | 0xC0 -> true
  | _ -> false

let is_sgb rom =
  let f = Bs.get rom Constants.sgb_flag in
  match Uint8.code f with
  | 0x03 -> true
  | _ -> false

let cartridge_kind rom =
  let f = Bs.get rom Constants.cartridge_flag in
  match Uint8.code f with
  | 0x00 -> `Rom_only
  | 0x01
  | 0x02
  | 0x03 -> `Mbc1
  | _ -> `Unimplemented

let rom_size rom =
  let f = Bs.get rom Constants.rom_size in
  (32000) lsl Uint8.code f

let ram_size rom =
  let f = Bs.get rom Constants.ram_size in
  match Uint8.code f with
  | 0x00 -> 0
  | 0x01 -> 2048
  | 0x02 -> 8192
  | 0x03 -> 32768
  | _ -> assert false
