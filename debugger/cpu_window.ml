open Goodboy
open Notty

let s = ""

let checkmark = I.string A.(fg white) "✔"
let image_bool b =
  if b then checkmark else
  I.string A.(fg white) ""

let make ({ cpu; gpu; mem; _ } as state : State.t) =
  let open Registers in
  let ss = I.string A.(fg white) in
  let pc = cpu.pc |> Utils.show_hex_i16 |> I.string A.(fg white) in
  let sp = cpu.sp |> Utils.show_hex_i16 |> I.string A.(fg white) in
  let rg = cpu.rg in
  let bc = get_pair rg `Bc |> Utils.show_hex_i16 |> I.string A.(fg white) in
  let de = get_pair rg `De |> Utils.show_hex_i16 |> I.string A.(fg white) in
  let hl = get_pair rg `Hl |> Utils.show_hex_i16 |> I.string A.(fg white) in
  let af = get_pair rg `Af |> Utils.show_hex_i16 |> I.string A.(fg white) in
  let z = image_bool (is_flag_set rg z) in
  let n = image_bool (is_flag_set rg n) in
  let hc = image_bool (is_flag_set rg hc) in
  let ca = image_bool (is_flag_set rg ca) in
  let in_bios = image_bool mem.in_bios in
  let if_ = Mmu.get_n state 0xFF0F |> Uint8.show_hex' |> I.string A.(fg white) in
  let ie = Mmu.get_n state 0xFFFF |> Uint8.show_hex' |> I.string A.(fg white) in
  let ime = image_bool cpu.ime in
  let t = cpu.t |> string_of_int |> I.string A.(fg white) in
  let ly = gpu.Gpu.ly |> Uint8.show_hex |> I.string A.(fg white) in
  let lcd_stats = gpu.Gpu.lcd_register |> Uint8.show_hex |> I.string A.(fg white) in
  let lcd_control = gpu.Gpu.control_register |> Uint8.show_hex |> I.string A.(fg white) in
  let ba = mem.cartridge.print_banks () |> I.string A.(fg white) in
  let desc = [
    ss "pc";
    ss "sp";
    ss "bc";
    ss "de";
    ss "hl";
    ss "af";
    ss "z";
    ss "n";
    ss "hc";
    ss "ca";
    ss "if";
    ss "ie";
    ss "in_bios";
    ss "ime";
    ss "t";
    ss "ly";
    ss "lcd_stats";
    ss "lcd_control";
    ss "ba";
  ] |> I.vcat |> I.hpad 2 1
  in
  let v = [
    pc;
    sp;
    bc;
    de;
    hl;
    af;
    z;
    n;
    hc;
    ca;
    if_;
    ie;
    in_bios;
    ime;
    t;
    ly;
    lcd_stats;
    lcd_control;
    ba;
  ] |> I.vcat in
  Window.with_window ~title:"registers" I.(desc <|> v)
