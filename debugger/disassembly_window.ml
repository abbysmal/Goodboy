open Notty
open Goodboy
open Pretty_opcodes

type t = int

let some s = Some s

let get_instr is_extended opcode =
  try
    if is_extended then
      some @@ get_ext_opcode_repr opcode
    else
      some @@ get_opcode_repr opcode
  with
  | Not_found -> None

let image_of_opcode addr v (o : opcode) =
  let addr = I.string A.(fg (gray 10)) (Utils.show_hex_i16 addr) in
  let opcode = I.string A.(fg yellow) (Utils.show_hex_i8 v) in
  let mnemo = I.string A.(fg green) o.mnemo in
  let operands =
    match o.operand1, o.operand2 with
    | None, None -> I.string A.(fg white) ""
    | Some o1, None -> I.string A.(fg white) (Printf.sprintf "%s" o1)
    | Some o1, Some o2 -> I.string A.(fg white) (Printf.sprintf "%s,%s" o1 o2)
    | _ -> assert false
  in
  let size = I.string A.(fg blue) (string_of_int o.size) in
  addr, opcode, mnemo, operands, size

let image_of_memory addr value =
  let addr = I.string A.(fg (gray 10)) (Utils.show_hex_i16 addr) in
  let value = I.string A.(fg yellow) (Uint8.show_hex value) in
  addr, value


let breakpoint_reached breakpoints i = List.exists (fun (b, _) -> i = b) breakpoints

let status_image is_extended pc breakpoints i =
  if i = pc then
    I.string A.(fg green) "■ "
  else if is_extended && (i + 1 = pc) then
    I.string A.(fg green) "■ "
  else if breakpoint_reached breakpoints i then
    I.string A.(fg red) "● "
  else
    I.void 2 1

let compile_instructions ~h ~pos ~breakpoints state =

  let init_array sz = Array.init sz (fun _ -> I.empty) in
  let get = Mmu.get_n state in

  let start, until =
    let start = pos - (h / 2) in
    let until = pos + (h / 2) in
    if start < 0 then
      0, h
    else if (pos + (h / 2)) > 0xFFFF then
      (0xFFFF - h), 0xFFFF
    else
      start, until
  in

  let sz = until - start + 1 in
  let status_i = init_array sz in
  let addr_i = init_array sz in
  let opcode_i = init_array sz in
  let mnemo_i = init_array sz in
  let operands_i = init_array sz in
  let size_i = init_array sz in

  let update_instr_arrays i status (addr, opcode, mnemo, operands, size) =
    status_i.(i) <- status;
    addr_i.(i) <- addr;
    opcode_i.(i) <- opcode;
    mnemo_i.(i) <- mnemo;
    operands_i.(i) <- operands;
    size_i.(i) <- size;
  in

  let update_memory_arrays i (addr, v) =
    addr_i.(i) <- addr;
    opcode_i.(i) <- v;
    mnemo_i.(i) <- I.string A.(fg white) "";
    operands_i.(i) <- I.string A.(fg white) "";
    size_i.(i) <- I.string A.(fg white) "";
    status_i.(i) <- I.void 2 1
  in

  let get_from_rom is_extended i cursor =
    let opcode = get cursor |> Uint8.code in
    let instr = get_instr is_extended opcode in
    match instr with
    | Some instr -> begin
      let status = status_image is_extended state.cpu.pc breakpoints cursor in
      image_of_opcode cursor opcode instr
      |> update_instr_arrays i status;
      let next_is_extended = if opcode = 0xCB && not is_extended then true else false in
      match instr.size with
      | 1 -> i + 1, cursor + 1, next_is_extended
      | 2 ->
        if (i + 1) < sz then
          image_of_memory (cursor + 1) (get (cursor + 1))
          |> update_memory_arrays (i + 1);
        i + 2, cursor + 2, next_is_extended
      | 3 ->
        if (i + 1) < sz then
          image_of_memory (cursor + 1) (get (cursor + 1))
          |> update_memory_arrays (i + 1);
        if (i + 2) < sz then
          image_of_memory (cursor + 2) (get (cursor + 2))
          |> update_memory_arrays (i + 2);
        i + 3, cursor + 3, next_is_extended
      | _ -> assert false
    end
    | None -> begin
      image_of_memory cursor (Uint8.chr opcode)
      |> update_memory_arrays i;
      i + 1, cursor + 1, false
    end

  in

  let rec walk is_extended i cur until =
    let open Mmu in
    match cur with
    | _ when cur >= until -> ()
    | _ when (is_between' cur Memory.cartridge
              || is_between' cur Memory.wram0
              || is_between' cur Memory.wram1) 
              ->
      let (i, cur, next_is_extended) = get_from_rom is_extended i cur in
      walk next_is_extended i cur until
    | _ ->
      image_of_memory cur (get cur)
      |> update_memory_arrays i;
      walk false (succ i) (cur + 1) until
  in

  walk false 0 start until;

  let col a = Array.fold_left I.(<->) I.empty a in

  let image = I.(
    col status_i
    <|> col addr_i |> hpad 2 5
    <|> col opcode_i |> hpad 0 5
    <|> col mnemo_i |> hpad 0 5
    <|> col operands_i |> hpad 0 5
    <|> col size_i
  )
  in
  Window.with_window ~title:"instructions" (I.vsnap (h - 10) image)

let help_window =
  let keys = [
   I.string A.(fg white) "P";
   I.string A.(fg white) "R";
   I.string A.(fg white) "S";
   I.string A.(fg white) "F1";
   I.string A.(fg white) "F2";
  ] |> I.vcat
  in
  let descr = [
    I.string A.(fg white) "Pause emulation";
    I.string A.(fg white) "Run";
    I.string A.(fg white) "Single step";
    I.string A.(fg white) "Switch to game window";
    I.string A.(fg white) "Switch to debugger view";
  ] |> I.vcat
  in
  I.(keys |> hpad 1 4 <|> descr) |> Window.with_window ~title:"halp"
