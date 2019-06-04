open Goodboy

let log fmt = Format.printf (fmt ^^ "@.")
let log_err fmt = Format.eprintf (fmt ^^ "@.")

module SdlEngine  : Engine.Engine_impl = struct

open Tsdl
open Result

type t = {
  win : Tsdl.Sdl.window;
  event : Tsdl.Sdl.event;
  mutable joypad_changed : bool;
  mutable quit : bool;
  mutable last_tick : Int32.t;
}

let teardown t =
  Sdl.destroy_window t.win;
  Sdl.quit ();
  exit 0

let update_joypad t _ =
  if t.joypad_changed then begin
    t.joypad_changed <- false;
    true
  end
  else
    false

let refresh ({ win; event; last_tick; _ } as t) state =
  let fb = state.State.gpu.framebuffer in
  let framebuffer = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout (160 * 144) in
  for y = 0 to 143 do
    for x = 0 to 159 do
      match fb.(x).(y) with
      | `White -> framebuffer.{(y * 160) + x} <- 0xFFFFFFl
      | `Black -> framebuffer.{(y * 160) + x} <- 0x000000l
      | `Light_gray -> framebuffer.{(y * 160) + x} <- 0xAAAAAAl
      | `Dark_gray -> framebuffer.{(y * 160) + x} <- 0x777777l
    done
  done;
  let open Rresult.R.Infix in
  let aux () =
    let ticks = Sdl.get_ticks () in
    let diff = Int32.(sub ticks last_tick) in
    if diff < (Int32.div 1000l 60l) then
      Sdl.delay Int32.(sub (Int32.div 1000l 60l) diff);
    t.last_tick <- ticks;
    Sdl.get_window_surface win >>= fun surface ->
    Sdl.create_rgb_surface ~w:160 ~h:144 ~depth:32
      0x000000l 0x000000l 0x000000l 0x000000l >>= fun sf ->
    Sdl.lock_surface sf >>= fun () ->
    let pixels = Sdl.get_surface_pixels sf Bigarray.int32 in
    Bigarray.Array1.blit framebuffer pixels;
    Sdl.unlock_surface sf;
    Sdl.blit_scaled ~src:sf None ~dst:surface None >>= fun () ->
    Sdl.update_window_surface win
  in
  match aux () with
  | _ -> ();

  match Sdl.poll_event (Some event) with
  | false -> ()
  | true ->
    match Sdl.Event.(enum (get event typ)) with
  | `Quit -> t.quit <- true
  | `Key_down ->
    begin
      let joypad = state.mem.joypad in
      t.joypad_changed <- true;
      let key = Sdl.(Event.(get event keyboard_keycode)) in
      let open Tsdl.Sdl.K in
      if key = up then
        Joypad.button_down joypad `Up
      else if key = down then
        Joypad.button_down joypad `Down
      else if key = right then
        Joypad.button_down joypad `Right
      else if key = left then
        Joypad.button_down joypad `Left
      else if key = a then
        Joypad.button_down joypad `Start
      else if key = s then
        Joypad.button_down joypad `Select
      else if key = q then
        Joypad.button_down joypad `A
      else if key = w then
        Joypad.button_down joypad `B
    end
  | `Key_up ->
    begin
      let joypad = state.mem.joypad in
      t.joypad_changed <- true;
      let key = Sdl.(Event.(get event keyboard_keycode)) in
      let open Tsdl.Sdl.K in
      if key = up then
        Joypad.button_up joypad `Up
      else if key = down then
        Joypad.button_up joypad `Down
      else if key = right then
        Joypad.button_up joypad `Right
      else if key = left then
        Joypad.button_up joypad `Left
      else if key = a then
        Joypad.button_up joypad `Start
      else if key = s then
        Joypad.button_up joypad `Select
      else if key = q then
        Joypad.button_up joypad `A
      else if key = w then
        Joypad.button_up joypad `B
    end
  | _ -> ()


let is_paused _ = false

let step t _ =
  match t with
  | { quit = true; _ } -> teardown t
  | _ -> t

let create state =
  let inits = Sdl.Init.(everything) in
  match Sdl.init inits with
  | Error (`Msg e) -> log_err "Sdl.init: %s" e; assert false
  | Ok () ->
    let flags = Sdl.Window.(shown + opengl) in
    match Sdl.create_window ~w:320 ~h:288 "Goodboy" flags with
    | Error (`Msg e) -> log_err "Sdl.create_window: %s" e; assert false
    | Ok win ->
      let event = Sdl.Event.create () in
      Sdl.start_text_input ();
      log "SDL started";
      {
        win;
        event;
        joypad_changed = false;
        quit = false;
        last_tick = Sdl.get_ticks ();
      }
    , state

end

let load_file path =
  let open Rresult.R.Infix in
  Fpath.of_string path
  >>= Bos.OS.File.read
  >>= fun content ->
  Ok (Bigstringaf.of_string ~off:0 ~len:(String.length content) content)

let main cartridge bootrom =
  let open Rresult.R.Infix in
  let module Main = Engine.Make(SdlEngine) in
  load_file cartridge >>= fun rom ->
  (match bootrom with
  | Some bootrom -> load_file bootrom >>= fun s -> Ok (Some s)
  | None -> Ok None)
  >>= fun boot ->
  let (e,s) = Main.make ?boot ~rom () in
  let s =
    match boot with
    | None -> Engine.initial_state s
    | Some _ -> s
  in
  let rec loop (t : Main.t) = loop (Main.step t) in
  loop (e, s)

let () =
  let open Cmdliner in
  Term.exit @@ Term.eval Term.(const main  $ Args.cartridge $ Args.bootrom, Args.info)
