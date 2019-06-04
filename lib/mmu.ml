let is_between v a b = v >= a && v <= b
let is_between' v (a, b) = v >= a && v <= b

exception Out_of_range of string
let out_of_range addr = raise (Out_of_range (Printf.sprintf "0x%04X" addr))
let out_of_range_e addr = raise (Out_of_range (Printf.sprintf "lol 0x%04X" addr))

  open Memory

let get_n { State.mem; gpu; timers;  _ } addr = try
  match addr with

  | 0xFF04 -> timers.div 
  | 0xFF05 -> timers.tima 
  | 0xFF06 -> timers.tma
  | 0xFF07 -> timers.tac 
  | 0xFF00 -> Joypad.get mem.joypad
  | 0xFF40 -> gpu.control_register
  | 0xFF41 -> gpu.lcd_register
  | 0xFF42 -> gpu.scr_y
  | 0xFF43 -> gpu.scr_x
  | 0xFF44 -> gpu.ly
  | 0xFF45 -> gpu.lyc
  | 0xFF46 -> gpu.dma_transfer
  | 0xFF47 -> gpu.bg_pal
  | 0xFF48 -> gpu.obj_pal0
  | 0xFF49 -> gpu.obj_pal1
  | 0xFF4B -> gpu.win_x
  | 0xFF4A -> gpu.win_y
  | a when is_between' a Memory.bios && mem.in_bios -> mem.bios.get a
  | a when is_between' a Memory.cartridge || is_between' a cartridge_ram ->
    mem.cartridge.get addr
  | a when is_between' a vram -> mem.vram.get a
  | a when is_between' a wram0 -> mem.wram0.get a
  | a when is_between' a wram1 -> mem.wram1.get a
  | a when is_between' a echo1 -> mem.wram0.get (a - 0x2000)
  | a when is_between' a echo2 -> mem.wram1.get (a - 0x2000)
  | a when is_between' a unused -> Uint8.chr 0xFF
  | a when is_between' a oam  -> mem.oam.get a
  | a when is_between' a io_ports -> mem.io_ports.get a
  | a when is_between' a hram -> mem.hram.get a
  | _ -> out_of_range_e addr
  with e -> (print_endline @@ Printexc.to_string e); raise e

let get_nn t addr =
  ((get_n t (addr + 1) |> Uint8.code) lsl 8)
  lor
  (get_n t (addr) |> Uint8.code)

exception Illegal_write of string
let raise_illegal_wr s = raise @@ Illegal_write s

let put_n ( t : State.t ) addr value = try
  match addr with

  | 0xFF40 -> t.gpu.control_register <- value
  | 0xFF41 ->
    t.gpu.lcd_register <-
      Uint8.((value land (chr 120))
             lor
             ( t.gpu.lcd_register land (chr 0b10000111)))
  | 0xFF42 -> t.gpu.scr_y <- value
  | 0xFF43 -> t.gpu.scr_x <- value
  | 0xFF44 -> t.gpu.ly <- value
  | 0xFF45 -> t.gpu.lyc <- value
  | 0xFF4B -> t.gpu.win_x <- value
  | 0xFF4A -> t.gpu.win_y <- value
  | 0xFF47 -> t.gpu.bg_pal <- value
  | 0xFF48 -> t.gpu.obj_pal0 <- value
  | 0xFF49 -> t.gpu.obj_pal1 <- value
  | 0xFF46 -> t.gpu.dma_transfer <- value; t.gpu.dma_transfer_request <- true
  | 0xFF02 ->
    if value = Uint8.chr 0x81 then
      t.mem.serial_out <- Some (get_n t 0xFF01)
    else
      ()
  | 0xFF00 -> Joypad.write t.mem.joypad value
  | 0xFF04 -> t.timers.div <- Uint8.zero 
  | 0xFF05 -> t.timers.tima <- value
  | 0xFF06 -> t.timers.tma <- value
  | a when is_between' a oam  -> t.mem.oam.set_n a value
  | 0xFF07 -> t.timers.tac <- Uint8.(value land chr 7)
  | a when is_between' a bios && t.mem.in_bios -> raise_illegal_wr "Tried to write to bios rom"
  | a when (is_between' a cartridge) || (is_between' a cartridge_ram) ->
    t.mem.cartridge.put_n addr value
  | a when is_between' a vram -> t.mem.vram.set_n a value
  | a when is_between' a wram0 -> t.mem.wram0.set_n a value
  | a when is_between' a wram1 -> t.mem.wram1.set_n a value
  | a when is_between' a echo1 -> ()
  | a when is_between' a echo2 -> ()
  | a when is_between' a unused -> ()
  | a when is_between' a io_ports -> t.mem.io_ports.set_n a value
  | a when is_between' a hram -> t.mem.hram.set_n a value
  | _ -> out_of_range_e addr
  with Invalid_argument _ -> out_of_range addr

let put_nn t addr v =
  put_n t (addr + 1) (v lsr 8 |> Uint8.chr);
  put_n t (addr) (v land 0x00FF |> Uint8.chr)

type region = [
  `Boot
  | `Rom0
  | `Rom1
  | `Vra0
  | `Sra0
  | `Wra0
  | `Wra1
  | `Ech0
  | `Ech1
  | `Oam
  | `Unk
  | `Io
  | `Hram
]

type dump = (int * region * Uint8.t list) list 

let select_region in_bios = function
  | n when is_between n 0x0 0x100 && in_bios -> `Boot
  | n when is_between n 0x0 0x3FFF -> `Rom0
  | n when is_between n 0x4000 0x7FFF -> `Rom1
  | n when is_between n 0x8000 0x9FFF -> `Vra0
  | n when is_between n 0xA000 0xBFFF -> `Sra0
  | n when is_between n 0xC000 0xCFFF -> `Wra0
  | n when is_between n 0xD000 0xDFFF -> `Wra1
  | n when is_between n 0xE000 0xEFFF -> `Ech0
  | n when is_between n 0xF000 0xFDFF -> `Ech1
  | n when is_between n 0xFE00 0xFE9F -> `Oam
  | n when is_between n 0xFEA0 0xFEFF -> `Unk
  | n when is_between n 0xFF00 0xFF7F -> `Io
  | n when is_between n 0xFF80 0xFFFF -> `Hram
  | _ -> raise Not_found

let dump (t : State.t) : dump =
  let rec aux i acc =
    if i >= 0xFFFF then
      acc
    else
      let n = List.init 16 (fun ii -> get_n t  (i + ii)) in
      let region = select_region t.mem.in_bios i in
      aux (i + 16) ((i, region, n)::acc)
  in
  aux 0 []
  |> List.rev
