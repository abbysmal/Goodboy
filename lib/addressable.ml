type uint8 = Uint8.t
type uint16 = int
module Bs = Bigstringaf

type t = {
  buffer : Bs.t;
  get : int -> uint8;
  set_n : int -> uint8 -> unit;
}

let make ?buffer (b, e) =
  let buffer = match buffer with Some buffer -> buffer | _ -> Bs.create (e - b + 1) in
  let get = fun addr -> Bs.get buffer (addr - b) in
  let set_n = fun addr v -> Bs.set buffer (addr - b) v in
  { buffer; set_n; get; }

let void () = {
  buffer = Bs.empty;
  get = (fun _ -> Uint8.chr 0xFF);
  set_n = (fun _ _ -> ());
}
