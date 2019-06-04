open Sexplib.Std

type t = Bigstringaf.t
type register = int
type flag = int
type paired_register = [ `Af | `Bc | `De | `Hl ] [@@deriving sexp]

let a = 0
let f = 1
let b = 2
let c = 3
let d = 4
let e = 5
let h = 6
let l = 7

let z = 7
let n = 6
let hc = 5
let ca = 4

let register_of_sexp _ = assert false
let sexp_of_register reg =
  let name =
    match reg with
    | 0 -> 'A'
    | 1 -> 'F'
    | 2 -> 'B'
    | 3 -> 'C'
    | 4 -> 'D'
    | 5 -> 'E'
    | 6 -> 'H'
    | 7 -> 'L'
    | _ -> '?'
  in
  sexp_of_char name

let flag_of_sexp _ = assert false
let sexp_of_flag flag =
  let name =
    match flag with
    | 4 -> 'Z'
    | 5 -> 'N'
    | 6 -> 'H'
    | 7 -> 'C'
    | _ -> '?'
  in
  sexp_of_char name

let make () =
  let t = Bigstringaf.create 8 in
  Bigstringaf.set t a '\000';
  Bigstringaf.set t f '\000';
  Bigstringaf.set t b '\000';
  Bigstringaf.set t c '\000';
  Bigstringaf.set t d '\000';
  Bigstringaf.set t e '\000';
  Bigstringaf.set t h '\000';
  Bigstringaf.set t l '\000';
  t

let get t register = Bigstringaf.get t register
let set t register = Bigstringaf.set t register

let get_pair t = function
  | `Af -> (Bigstringaf.get_int16_be t a) land 0xFFF0
  | `Bc -> Bigstringaf.get_int16_be t b
  | `De -> Bigstringaf.get_int16_be t d
  | `Hl -> Bigstringaf.get_int16_be t h

let set_pair t = function
  | `Af -> Bigstringaf.set_int16_be t (a land 0xFFF0)
  | `Bc -> Bigstringaf.set_int16_be t b
  | `De -> Bigstringaf.set_int16_be t d
  | `Hl -> Bigstringaf.set_int16_be t h

let set_flag t flag =
  let flags = get t f in
  set t f (Uint8.set_bit flags flag)

let unset_flag t flag =
  let flags = get t f in
  set t f (Uint8.unset_bit flags flag)

let toggle_flag t flag =
  let flags = get t f in
  set t f (Uint8.toggle_bit flags flag)

let is_flag_set t flag =
  let flags = get t f in
  Uint8.is_bit_set flags flag

let clear_flags t =
  unset_flag t z;
  unset_flag t n;
  unset_flag t hc;
  unset_flag t ca

let write_flag registers flag = function true -> set_flag registers flag | false -> unset_flag registers flag

let write_flags ?z:f1 ?n:f2 ?hc:f3 ?ca:f4 r =
  let iter_opt f = function
    | Some v -> f v
    | None -> ()
  in
  iter_opt (write_flag r z) f1;
  iter_opt (write_flag r n) f2;
  iter_opt (write_flag r hc) f3;
  iter_opt (write_flag r ca) f4
