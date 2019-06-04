type t = {
  cpu : Cpu.t;
  mem : Memory.t;
  gpu : Gpu.t;
  timers : Timers.t;
}

let make ?boot ~rom () = {
  cpu = Cpu.make ();
  mem = Memory.make ?boot ~rom;
  gpu = Gpu.make ();
  timers = Timers.make ();
}
