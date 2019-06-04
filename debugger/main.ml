open Goodboy

module type Params = sig

  val bootrom : string
  val cartridge : string
  val breakpoints : (int * int) list

end

let load_file path =
  let open Rresult.R.Infix in
  match Fpath.of_string path >>= Bos.OS.File.read with
  | Ok content -> Bigstringaf.of_string ~off:0 ~len:(String.length content) content
  | _ -> raise Not_found

module Debugger (P : Params) : Engine.Engine_impl = struct

module Button = struct

  type t = Joypad.button * int

  let compare (a, _) (b, _) = compare a b
      
end
module S = Set.Make(Button)
  
type windows =
    Display
  | Debugger

type t = {
  h : int;
  w : int;
  stopped : bool;
  single_step : bool;
  breakpoints : (int * int) list;
  current_window : windows;
  hexdump_window : Window.t;
  term : Notty_unix.Term.t;
  fd : Unix.file_descr list;
  keys : S.t;
}

let create state =
  let breakpoints = P.breakpoints in
  let h = 72 in
  let w = 160 in
  let term =  Notty_unix.Term.create () in
  let fd, _ = Notty_unix.Term.fds term in
  let hexdump_window =
    Window.make
      ~cursor:0
      ~title:"hexdump"
      ~refresh:(fun state -> Hexdump_window.make state)
      state
  in
  let fd = [fd] in
  {
    keys = S.empty;
    term;
    fd;
    single_step = false;
    breakpoints;
    w;
    h;
    hexdump_window;
    stopped = false;
    current_window = Display;
  }, state

let teardown t = Notty_unix.Term.release t.term

let white = Notty.A.white
let light_gray = Notty.A.(gray 3)
let dark_gray = Notty.A.(gray 10)
let black = Notty.A.black

let is_paused t = if t.single_step then false else t.stopped

let halfblock = "▄"

let pxmatrix w h f = Notty.I.tabulate w h @@ fun x y ->
  let y = y * 2 in
  Notty.I.string Notty.A.(bg (f x y) ++ fg (f x (y + 1))) halfblock

let to_notty_color = function
  | `White -> white
  | `Light_gray -> light_gray
  | `Dark_gray -> dark_gray
  | `Black -> black

let draw_game_window t (state : State.t) =
  let image = pxmatrix 160 72 (fun x y -> to_notty_color state.gpu.framebuffer.(x).(y)) in
  Notty_unix.Term.image t.term image;
  Notty_unix.Term.refresh t.term

let refresh t state =
  match t.current_window, t.stopped with
  | Display, false -> draw_game_window t state
  | _ -> ()

let update_joypad (t : t) joypad =
  S.fold begin fun k j ->
    match k with
    | (key, 500) ->
      Joypad.button_down joypad key;
      true
    | (key, 0) ->
      Joypad.button_up joypad key;
      true
    | _ -> j
  end t.keys false

let breakpoint_reached breakpoints state = List.exists begin
    fun (b, o) ->

    state.State.cpu.pc = b && (Mmu.get_n state state.cpu.pc = Uint8.chr o)
  end breakpoints

let add_key keys b  = S.add (b, 510) keys
    
let check_event t (state : State.t) =
  match Notty_unix.Term.event t.term, t.current_window with
  | `Key (`Escape, []), _ -> raise Exit
  | `Key (`ASCII 'g',  []), Display -> begin
      Mmu.put_n state 0xC0A2 (Uint8.chr 100);
      Mmu.put_n state 0xC0A1 (Uint8.chr 100);
      Mmu.put_n state 0xC0A0 (Uint8.chr 100);
      t
    end
  | `Key (`ASCII 'h',  []), Display -> begin
      Mmu.put_n state 0xFFE1 (Uint8.chr 0x22);
      t
    end
  | `Key (`ASCII 'q', []), Display -> { t with keys = add_key t.keys `A; }
  | `Key (`ASCII 'w', []), Display -> { t with keys = add_key t.keys `B; }
  | `Key (`ASCII 'a', []), Display -> { t with keys = add_key t.keys `Start; }
  | `Key (`ASCII 's', []), Display -> { t with keys = add_key t.keys `Select; }
  | `Key (`Arrow arrow, []), Display -> { t with keys = add_key t.keys (arrow :> Joypad.button); }
  | `Key (`Page `Up, _), Debugger ->
    { t with hexdump_window = Window.down_cursor t.hexdump_window 50; }
  | `Key (`Page `Down, _), Debugger ->
    { t with hexdump_window = Window.up_cursor t.hexdump_window 50; }
  | `Key (`Arrow `Down, []), Debugger ->
    { t with hexdump_window = Window.up_cursor t.hexdump_window 1; }
  | `Key (`Arrow `Up, []), Debugger ->
      { t with hexdump_window = Window.down_cursor t.hexdump_window 1; }
  | `Key (`Function 2, _), _ ->
    { t with
      current_window = Debugger;
      stopped = true;
      hexdump_window = Window.refresh t.hexdump_window state;
    }
  | `Key (`Function 1, _), _ -> { t with current_window = Display; stopped = false; }
  | `Key (`ASCII 's', _), _ ->
    { t with
      single_step = true;
      hexdump_window = Window.refresh t.hexdump_window state;
    }
  | `Key (`ASCII 'p', _), _ -> { t with stopped = true; }
  | `Key (`ASCII 'r', _), _ -> { t with stopped = false; current_window = Display; }
  | _ -> t

let check_input t state =
  (* FIXME: this select is evil *)
  match t.current_window, t.stopped with
  | Display, _ -> begin
    match Unix.select t.fd [] [] 0.0000001 with
    | (_::[]), _, _ -> check_event t state
    | _ -> t
  end
  | _, _ -> check_event t state

let display_debugger t (state : State.t) =
  let hexdump = Window.render ~window_h:50 t.hexdump_window in
  let disassembly =
    Disassembly_window.compile_instructions
      ~breakpoints:t.breakpoints
      ~h:t.h
      ~pos:state.cpu.pc
      state
  in
  let cat = Cat_window.cat_image state.cpu.pc in
  let cpu = Cpu_window.make state in
  let img = Notty.I.(disassembly <|> (cat <-> Help_window.help_window <-> cpu) <|> hexdump) in
  Notty_unix.Term.image t.term img;
  Notty_unix.Term.refresh t.term

let step t state =
  let t =
    match t.current_window, t.stopped with
    | Display, _ -> begin
        match breakpoint_reached t.breakpoints state with
        | true ->
          let t =
            { t with
              stopped = true;
              current_window = Debugger;
            }
          in
          display_debugger t state;
          t
        | false -> t
      end
    | Debugger, _ ->
      let t = { t with single_step = false; } in
      display_debugger t state;
      t
  in
  let t = check_input t state in
  let keys =
    S.map (fun (k, i) -> (k, pred i)) t.keys
    |> S.filter (fun (_, i) -> i >= 0)
  in
  { t with keys; }
  
end

exception Exit
let main breakpoints cartridge =
  let module Params = struct
    let breakpoints = breakpoints
    let cartridge = cartridge
    let bootrom = "assets/bootrom.gb"
  end
  in
  let module Debugger = Debugger(Params) in
  let module Main = Engine.Make(Debugger) in
  let rom = load_file Params.cartridge in
  let boot = load_file Params.bootrom in
  let t = Main.make ~rom ~boot () in
  let rec loop (t : Main.t) =
    match Main.step t with
    | exception exn ->
      Debugger.teardown (fst t);
      Printf.eprintf "exception %s pc: %04X val: %02X\n"
		    (Printexc.to_string exn)
        (snd t).State.cpu.pc
        (Mmu.get_n (snd t) (snd t).State.cpu.pc |> Uint8.code)
    | t -> loop t
  in
  loop t

let () =
  let open Cmdliner in
  Term.exit @@ Term.eval Term.(const main $ Args.breakpoints $ Args.cartridge, Args.info)
