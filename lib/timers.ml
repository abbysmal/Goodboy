type t = {
  mutable main : int;
  mutable sub : int;
  mutable div_c : int;
  mutable tima : Uint8.t;
  mutable div : Uint8.t;
  mutable tma : Uint8.t;
  mutable tac : Uint8.t;
}


let make () = {
  main = 0;
  sub = 0;
  div_c = 0;
  div = Uint8.zero;
  tima = Uint8.zero;
  tma = Uint8.zero;
  tac = Uint8.zero;
}

let get_mode { tac; _ } =
  if Uint8.code tac land 4 != 0 then
    match Uint8.code tac land 3 with
    | 0 -> Some 64
    | 1 -> Some 1
    | 2 -> Some 4
    | 3 -> Some 16
    | _ -> None
  else
    None

let tick t m_cycles =
  t.sub <- t.sub + m_cycles;
  if t.sub >= 4 then begin
    t.main <- succ t.main;
    t.sub <- t.sub - 4;
    t.div_c <- succ t.div_c;
    if t.div_c == 16 then begin

      t.div <- Uint8.succ t.div;
      t.div_c <- 0
    end
  end;
  match get_mode t with
  | None -> false
  | Some threshold when t.main > threshold -> begin
      t.main <- 0;
      t.tima <- Uint8.succ t.tima;
      if t.tima = Uint8.zero then begin
        t.tima <- t.tma;
        true
      end
      else
        false
    end
  | _ -> false
