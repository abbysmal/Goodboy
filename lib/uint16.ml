let max_int = 65_535

let add a b = (a + b) land max_int
let sub a b = (a - b) land max_int

let is_bit_set x bit = x land (1 lsl bit) != 0
