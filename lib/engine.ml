module type Engine_impl = sig

  type t

  val step : t -> State.t -> t
  val create : State.t -> (t * State.t)
  val refresh : t -> State.t -> unit
  val update_joypad : t -> Joypad.t -> bool
  val teardown : t -> unit
  val is_paused : t -> bool

end

module type Engine = sig

  type t

  val make : ?boot:Bigstringaf.t -> rom :Bigstringaf.t -> unit -> t
  val step : t -> t

end

module Make (M : Engine_impl) : (Engine with type t = (M.t * State.t)) = struct

  type t = M.t * State.t

  let make ?boot ~rom () =
    let state = State.make ?boot ~rom () in
    M.create state

  let step (t, state) =
    if M.is_paused t then
      let t = M.step t state in
      (t, state)
    else
    try
      let (state, _) = Cpu_exec.step state in
      let t = M.step t state in
      if state.gpu.redraw then begin
        M.refresh t state;
        state.gpu.redraw <- false
      end;
      match M.update_joypad t state.mem.joypad with
      | false -> (t, state)
      | true ->
        let isf = Mmu.get_n state 0xFF0F in
        Mmu.put_n state 0xFF0F (Uint8.set_bit isf Interrupts.joypad);
        (t, state)
    with
    | exn ->
      M.teardown t;
      Printf.printf "exception: %s" (Printexc.to_string exn);
      assert false

end

let initial_state (t : State.t) =
  let set a v = Mmu.put_n t a (Uint8.chr v) in
  Cpu.set_register_pair t.cpu `Af 0x01B0;
  Cpu.set_register_pair t.cpu `Bc 0x0013;
  Cpu.set_register_pair t.cpu `De 0x00D8;
  Cpu.set_register_pair t.cpu `Hl 0x014D;
  let sp = 0xFFFE in
  let pc = 0x0100 in
  set 0xFF05 0x00;
  set 0xFF06 0x00;
  set 0xFF07 0x00;
  set 0xFF10 0x80;
  set 0xFF11 0xBF;
  set 0xFF12 0xF3;
  set 0xFF14 0xBF;
  set 0xFF16 0x3F;
  set 0xFF17 0x00;
  set 0xFF19 0xBF;
  set 0xFF1A 0x7F;
  set 0xFF1B 0xFF;
  set 0xFF1C 0x9F;
  set 0xFF1E 0xBF;
  set 0xFF20 0xFF;
  set 0xFF21 0x00;
  set 0xFF22 0x00;
  set 0xFF23 0xBF;
  set 0xFF24 0x77;
  set 0xFF25 0xF3;
  set 0xFF26 0xF1;
  set 0xFF40 0x91;
  set 0xFF42 0x00;
  set 0xFF43 0x00;
  set 0xFF45 0x00;
  set 0xFF47 0xFC;
  set 0xFF48 0xFF;
  set 0xFF49 0xFF;
  set 0xFF4A 0x00;
  set 0xFF4B 0x00;
  set 0xFFFF 0x00;
  {
    t with
    cpu = {
      t.cpu with sp; pc;
    }
  }
