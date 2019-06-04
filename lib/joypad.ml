open Uint8

type t = {
  mutable control : Uint8.t;
  mutable arrows : Uint8.t;
  mutable buttons : Uint8.t;
}

type button = [ `A | `B | `Start | `Select | `Up | `Down | `Left | `Right ]

let address = 0xFF00

let p15 = 5
let p14 = 4

let a = 0
let b = 1
let select = 2
let start = 3
  
let right = 0
let left = 1
let up = 2
let down = 3

let make () = {
  control = Uint8.chr 0b11000000;
  arrows = Uint8.chr 0b1111;
  buttons =  Uint8.chr 0b1111; }

let write t b =
  let open Uint8 in
  t.control <- b lor chr 0b11000000

let button_index = function
  | `A -> a
  | `Right -> right
  | `B -> b
  | `Left -> left
  | `Select -> select
  | `Up -> up
  | `Start -> start
  | `Down -> down

let button_up t button =
  match button with
  | `A
  | `B
  | `Start
  | `Select ->
    t.buttons <- set_bit t.buttons (button_index button)
  | _ ->
    t.arrows <- set_bit t.arrows (button_index button)

let button_down t button =
  match button with
  | `A
  | `B
  | `Start
  | `Select ->
    t.buttons <- unset_bit t.buttons (button_index button)
  | _ ->
    t.arrows <- unset_bit t.arrows (button_index button)

let get t =
  let open Uint8 in
  let high = t.control in
  let low =
    let input = ref zero in
    if is_bit_set t.control p14 then
      input := !input lor t.buttons;
    if is_bit_set t.control p15 then
      input := !input lor t.arrows;
    !input
  in
  (high lor low)
