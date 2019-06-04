open Registers

type t = {
  sp : int;
  pc : int;
  rg : Registers.t;
  ime : bool;
  t : int;
  m : int;
  halted : bool;
}

let make () = {
  sp = 0;
  pc = 0;
  rg = Registers.make ();
  ime = false;
  t = 0;
  m = 0;
  halted = false;
}

let get_register { rg; _ } r = get rg r
let set_register { rg; _ } r = set rg r

let get_register_pair { rg; _ } r = get_pair rg r
let set_register_pair { rg; _ } r = set_pair rg r

let tick cpu n =
  let t = cpu.t + n in
  let m = cpu.m + (n / 4) in
  { cpu with t; m; }
