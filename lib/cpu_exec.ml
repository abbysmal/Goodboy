open Cpu

open State

let handle_interrupts ( { cpu; _ } as state ) =
  let open Uint8 in
  let dispatch state isf int handler = 
    let ime = false in
    let sp = state.cpu.sp - 2 in
    Mmu.put_nn state sp cpu.pc;
    let pc = handler in
    let halted = false in
    Mmu.put_n state 0xFF0F (unset_bit isf int);
    { state with cpu = { cpu with ime; sp; pc; halted; }; }
  in
  match cpu.ime with
  | false -> state
  | true ->
    let ise = Mmu.get_n state 0xFFFF in
    let isf = Mmu.get_n state 0xFF0F in
    let interrupt_on int ise isf =
      is_bit_set ise int
      &&
      is_bit_set isf int
    in
    if isf = zero then
      state
    else
      if interrupt_on Interrupts.vblank ise isf then
        dispatch state isf Interrupts.vblank Interrupts.vblank_handler
      else
      if interrupt_on Interrupts.lcd_stats ise isf then
        dispatch state isf Interrupts.lcd_stats Interrupts.lcd_stats_handler
      else
      if interrupt_on Interrupts.timer ise isf then
        dispatch state isf Interrupts.timer Interrupts.timer_handler
      else
      if interrupt_on Interrupts.joypad ise isf then
        dispatch state isf Interrupts.joypad Interrupts.joypad_handler
      else
        state

let step ({ cpu; _ } as state) =
  let cur = Mmu.get_n state cpu.pc in
  if cpu.pc = 0x100 then
    state.mem.in_bios <- false;
  let state, cycles =
    match cpu.halted with
    | true -> state, 4
    | false ->
      try
        match Char.code cur with
        | 0xCB ->
          let cur = Mmu.get_n state (cpu.pc + 1) in
          let op = Instructions.compile_extended cur in
          Alu.dispatch { state with cpu = { cpu with pc = cpu.pc + 2 }; } op
        | code ->
          let op = Instructions.compile code in
          try
            Alu.dispatch { state with cpu = { cpu with pc = succ cpu.pc; }; } op
          with
          | Not_found ->
            assert false
      with
      | exn ->
        failwith (Printf.sprintf "exception at %04X (%02X): %s" cpu.pc (Uint8.code cur) (Printexc.to_string exn))
  in
  let state = { state with cpu = Cpu.tick state.cpu cycles; } in
  if Timers.tick state.timers (cycles / 4) then begin
    let isf = Mmu.get_n state 0xFF0F in
    Mmu.put_n state 0xFF0F (Uint8.set_bit isf Interrupts.timer)
  end;
  let state = Gpu_exec.step state cycles in
  handle_interrupts state , cycles
