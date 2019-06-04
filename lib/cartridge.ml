module Bs = Bigstringaf

module I = Cartridge_internals

let is_between v a b = v >= a && v <= b
let is_between' v (a, b) = v >= a && v <= b

type t = {
  get : int -> Uint8.t;
  put_n : int -> Uint8.t -> unit;
  print_banks : unit -> string;
}

let make_mbc0 t =
  let get = function
    | addr when is_between addr 0x0000 0x7FFF -> Bs.get t addr
    | addr when is_between addr 0xA000 0xBFFF -> Uint8.zero
    | _ -> assert false
  in
  let put_n _ _ = ()
  in
  let print_banks _ = ""
  in
  {
    get;
    put_n;
    print_banks;
  }

let make_mbc1 rom =

  let ram_size = I.ram_size rom in
  let rom_size = Bigstringaf.length rom in
  let ram = Bs.create ram_size in

  let bank1 = ref 1 in
  let bank2 = ref 0 in
  let mode = ref false in
  let ram_enabled = ref false in

  let ram_offset' () =
    let bank = if !mode then !bank2 else 0x0 in
    0x2000 * bank in

  let ram_offset = ref 0 in

  let put_n addr v  =

    match addr with
    | addr when is_between addr 0x0000 0x1FFF ->
      if Uint8.code v land  0b1111 = 0b1010 then
        ram_enabled := true
      else
        ram_enabled := false

    | addr when is_between addr 0x2000 0x3FFF ->
      bank1 :=
        if Uint8.code v land 0b1_1111 == 0b0_0000 then
          0b0_0001
        else
          Uint8.code v land 0b1_1111;

    | addr when is_between addr 0x4000 0x5FFF -> begin
        bank2 := Uint8.code v land 0b11;
        ram_offset := ram_offset' ()
      end
    | addr when is_between addr 0x6000 0x7FFF -> begin
        mode := (Uint8.code v land 0b1) == 0b1;
        ram_offset := ram_offset' ();
    end
    | addr when is_between addr 0xA000 0xBFFF -> begin
        match !ram_enabled with
        | false -> ()
        | true when !mode = false -> Bs.set ram ((addr - 0xA000) land (Bs.length ram - 1)) v
        | true ->
          let addr = (!ram_offset lor (addr land 0x1fff)) land (Bs.length ram - 1) in
          Bs.set ram addr v
      end
    | _ -> assert false
  in

  let print_banks () =
    let mode = match !mode with true -> 1 | false -> 0 in
    Printf.sprintf "bank1: %02X bank2: %02X mode: %d" (!bank1) (!bank2) mode
  in

  let get = function
    | addr when is_between addr 0x0000 0x3FFF -> Bs.get rom addr
    | addr when is_between addr 0x4000 0x7FFF ->
      let rom_bank =
        let bank1 = if !bank1 = 0 then 1 else !bank1 in
        match !mode with
        | false -> ((!bank2 lsl 5) lor (bank1 land 0b11111))
        | true -> (0 lor (bank1 land 0b11111))
      in
      let addr' = (rom_bank lsl 14) lor (addr land 0b11111111111111)  in
      let addr' = addr' land (rom_size - 1) in
      Bs.get rom addr'
    | addr when is_between addr 0xA000 0xBFFF -> begin
        match !ram_enabled with
        | false -> Uint8.chr 0xFF
        | true when !mode = false  ->
          Bs.get ram ((addr - 0xA000) land (Bs.length ram - 1))
        | true ->
          let addr = (!ram_offset lor (addr land 0x1fff)) land (Bs.length ram - 1) in
          Bs.get ram addr
    end
    | _ -> Uint8.chr 0xFF
  in

  {
    get;
    put_n;
    print_banks;
  }

let make ~rom =
  match I.cartridge_kind rom with
  | `Rom_only -> make_mbc0 rom
  | `Mbc1 -> make_mbc1 rom
  | `Unimplemented -> assert false
