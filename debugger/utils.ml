open Goodboy
open Notty
open Printf

let msg s = Error (`Msg s)

let a_current = A.(fg black ++ bg green)
let show_hex_i16 i16 = sprintf "0x%04X" i16
let show_hex_i8 i8 = sprintf "0x%02X" i8
let show_instr i = (Instructions.sexp_of_instruction i |> Sexplib.Sexp.to_string)
