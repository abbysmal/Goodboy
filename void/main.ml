open Goodboy

let load_file path =
  let open Rresult.R.Infix in
  match Fpath.of_string path >>= Bos.OS.File.read with
  | Ok content -> Bigstringaf.of_string ~off:0 ~len:(String.length content) content
  | _ -> raise Not_found

let show_hex_i16 d = Printf.sprintf "%04x" d

let log ({ State.cpu; mem; _ } as t) =
  let open Registers in
  let pc = cpu.pc |> show_hex_i16 in
  let sp = cpu.sp |> show_hex_i16 in
  let rg = cpu.rg in
  let a = Printf.sprintf "%02X" (get rg a |> Uint8.code) in
  let bc = Printf.sprintf "%04X" (get_pair rg `Bc) in
  let de = get_pair rg `De |> show_hex_i16 in
  let hl = get_pair rg `Hl |> show_hex_i16 in
  let z = if is_flag_set rg z then 'Z' else '-' in
  let c = if is_flag_set rg ca then 'C' else '-' in
  let n = if is_flag_set rg n then 'N' else '-' in
  let h = if is_flag_set rg hc then 'H' else '-' in
  let i = Printf.sprintf "0x%02x" (Mmu.get_n t cpu.pc |> Uint8.code) in
  let ba = mem.cartridge.print_banks () in
  let d = Printf.sprintf "%02X" (Mmu.get_n t 0xA000 |> Uint8.code) in
  Printf.printf "A:%s F:%c%c%c%c BC:%s DE:%s HL:%s SP:%s PC:%s %s %s A000: %s\n" a z n h c bc de hl sp pc i ba d
module VoidEngine : Engine.Engine_impl = struct

  type t = unit

  let create state = (), state

  let teardown _ = ()

  let update_joypad _ _ = false

  let refresh _ _ = ()

  let is_paused _ = false
    
  let step _ s =
    if not s.State.mem.in_bios then log s
    (* match s.State.mem.serial_out with
     * | None -> ()
     * | Some c ->
     *   begin
     *     Printf.printf "%c%!" c;
     *     s.State.mem.serial_out <- None
     *   end *)

end

let main cartridge =
  let module Main = Engine.Make(VoidEngine) in
  let rom = load_file cartridge in
  let boot = load_file "assets/bootrom.gb" in
  let (e,s) = Main.make ~rom ~boot () in
  Mmu.put_n s 0xDF6A (Uint8.chr 0xbf);
  let rec loop (t : Main.t) = loop (Main.step t) in
  loop (e, s)

let () =
  let open Cmdliner in
  Term.exit @@ Term.eval Term.(const main  $ Args.cartridge, Args.info)
