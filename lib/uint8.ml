type t = char

let to_signed c =
  let c = Char.code c in
  if (c > 127) then
    -((lnot c+1) land 255)
  else
    c

let zero = Char.unsafe_chr 0
let one = Char.unsafe_chr 1

let max_int = 255

let code = Char.code
let chr = Char.unsafe_chr

let add x y = (code x + code y) land max_int |> chr
let sub x y = (code x - code y) land max_int |> chr
let mul x y = (code x * code y) land max_int |> chr
let div x y = code x / code y |> chr
let rem x y = code x mod code y |> chr
let pred x = sub x one
let succ x = add x one

module Infix = struct

  let ( * ) = mul
  let ( + ) = add
  let ( - ) = sub
  let ( / ) = div
  let ( mod ) = rem
    
end

let ( lsl ) x y = (code x lsl code y) land max_int |> chr
let ( lsr ) x y = code x lsr code y |> chr
let ( land ) x y = code x land code y |> chr
let ( lor ) x y = code x lor code y |> chr
let ( lxor ) x y = code x lxor code y |> chr
let lnot x = x lxor (Char.chr max_int)

let swap_nibbles x = ((x land (chr 0x0f)) lsl chr 4) lor ((x land (Char.chr 0xF0)) lsr (chr 4))

let is_bit_set x bit = x land (one lsl (Char.unsafe_chr bit)) != zero
let is_bit_set' bit x = is_bit_set x bit
let set_bit x bit = x lor (one lsl (Char.unsafe_chr bit))
let set_bit' bit x = x lor (one lsl (Char.unsafe_chr bit))
let unset_bit x bit = x land (lnot (one lsl (Char.unsafe_chr bit)))
let unset_bit' bit x = x land (lnot (one lsl (Char.unsafe_chr bit)))
let toggle_bit x bit = x lxor (one lsl (Char.unsafe_chr bit))


let show_hex x = Printf.sprintf "0x%02X" (code x)
let show_hex' x = Printf.sprintf "%02X" (code x)

let show_bin d =
  if d = zero then "0" else
  let rec aux acc d =
    if d = zero then acc else
    aux (string_of_int (code (d land one)) :: acc) (d lsr one)
  in
  String.concat "" (aux [] d)

(* let get_n_bits x n = (x lsr n) land (sub (Char.chr 8) n) *)
let get_n_bits x n = (x lsl n) lsr n
