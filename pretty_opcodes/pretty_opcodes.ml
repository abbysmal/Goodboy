type opcode = {
  mnemo : string;
  operand_count : int;
  operand1 : string option;
  operand2 : string option;
  size : int;
  cycles : string;
}

let show_hex_i8 i8 = Printf.sprintf "0x%02X" i8

let get_opcode_repr = function
| 0x3e -> { mnemo = "LD"; operand_count = 2; operand1 = Some "A"; operand2 = Some "d8"; size = 2; cycles = "8"; }
| 0x3d -> { mnemo = "DEC"; operand_count = 1; operand1 = Some "A"; operand2 = None; size = 1; cycles = "4"; }
| 0xe6 -> { mnemo = "AND"; operand_count = 1; operand1 = Some "d8"; operand2 = None; size = 2; cycles = "8"; }
| 0x3f -> { mnemo = "CCF"; operand_count = 0; operand1 = None; operand2 = None; size = 1; cycles = "4"; }
| 0x3a -> { mnemo = "LD"; operand_count = 2; operand1 = Some "A"; operand2 = Some "(HL-)"; size = 1; cycles = "8"; }
| 0x3c -> { mnemo = "INC"; operand_count = 1; operand1 = Some "A"; operand2 = None; size = 1; cycles = "4"; }
| 0x3b -> { mnemo = "DEC"; operand_count = 1; operand1 = Some "SP"; operand2 = None; size = 1; cycles = "8"; }
| 0xfb -> { mnemo = "EI"; operand_count = 0; operand1 = None; operand2 = None; size = 1; cycles = "4"; }
| 0xdc -> { mnemo = "CALL"; operand_count = 2; operand1 = Some "C"; operand2 = Some "a16"; size = 3; cycles = "24/12"; }
| 0x28 -> { mnemo = "JR"; operand_count = 2; operand1 = Some "Z"; operand2 = Some "r8"; size = 2; cycles = "12/8"; }
| 0x29 -> { mnemo = "ADD"; operand_count = 2; operand1 = Some "HL"; operand2 = Some "HL"; size = 1; cycles = "8"; }
| 0xcf -> { mnemo = "RST"; operand_count = 1; operand1 = Some "08H"; operand2 = None; size = 1; cycles = "16"; }
| 0xf9 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "SP"; operand2 = Some "HL"; size = 1; cycles = "8"; }
| 0xfa -> { mnemo = "LD"; operand_count = 2; operand1 = Some "A"; operand2 = Some "(a16)"; size = 3; cycles = "16"; }
| 0x22 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "(HL+)"; operand2 = Some "A"; size = 1; cycles = "8"; }
| 0x23 -> { mnemo = "INC"; operand_count = 1; operand1 = Some "HL"; operand2 = None; size = 1; cycles = "8"; }
| 0x20 -> { mnemo = "JR"; operand_count = 2; operand1 = Some "NZ"; operand2 = Some "r8"; size = 2; cycles = "12/8"; }
| 0x21 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "HL"; operand2 = Some "d16"; size = 3; cycles = "12"; }
| 0x26 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "H"; operand2 = Some "d8"; size = 2; cycles = "8"; }
| 0x27 -> { mnemo = "DAA"; operand_count = 0; operand1 = None; operand2 = None; size = 1; cycles = "4"; }
| 0x24 -> { mnemo = "INC"; operand_count = 1; operand1 = Some "H"; operand2 = None; size = 1; cycles = "4"; }
| 0x25 -> { mnemo = "DEC"; operand_count = 1; operand1 = Some "H"; operand2 = None; size = 1; cycles = "4"; }
| 0xba -> { mnemo = "CP"; operand_count = 1; operand1 = Some "D"; operand2 = None; size = 1; cycles = "4"; }
| 0xe2 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "(C)"; operand2 = Some "A"; size = 1; cycles = "8"; }
| 0x7d -> { mnemo = "LD"; operand_count = 2; operand1 = Some "A"; operand2 = Some "L"; size = 1; cycles = "4"; }
| 0x35 -> { mnemo = "DEC"; operand_count = 1; operand1 = Some "(HL)"; operand2 = None; size = 1; cycles = "12"; }
| 0x34 -> { mnemo = "INC"; operand_count = 1; operand1 = Some "(HL)"; operand2 = None; size = 1; cycles = "12"; }
| 0x37 -> { mnemo = "SCF"; operand_count = 0; operand1 = None; operand2 = None; size = 1; cycles = "4"; }
| 0x36 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "(HL)"; operand2 = Some "d8"; size = 2; cycles = "12"; }
| 0x31 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "SP"; operand2 = Some "d16"; size = 3; cycles = "12"; }
| 0x30 -> { mnemo = "JR"; operand_count = 2; operand1 = Some "NC"; operand2 = Some "r8"; size = 2; cycles = "12/8"; }
| 0x33 -> { mnemo = "INC"; operand_count = 1; operand1 = Some "SP"; operand2 = None; size = 1; cycles = "8"; }
| 0x32 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "(HL-)"; operand2 = Some "A"; size = 1; cycles = "8"; }
| 0x39 -> { mnemo = "ADD"; operand_count = 2; operand1 = Some "HL"; operand2 = Some "SP"; size = 1; cycles = "8"; }
| 0x38 -> { mnemo = "JR"; operand_count = 2; operand1 = Some "C"; operand2 = Some "r8"; size = 2; cycles = "12/8"; }
| 0xc0 -> { mnemo = "RET"; operand_count = 1; operand1 = Some "NZ"; operand2 = None; size = 1; cycles = "20/8"; }
| 0xe1 -> { mnemo = "POP"; operand_count = 1; operand1 = Some "HL"; operand2 = None; size = 1; cycles = "12"; }
| 0x88 -> { mnemo = "ADC"; operand_count = 2; operand1 = Some "A"; operand2 = Some "B"; size = 1; cycles = "4"; }
| 0x89 -> { mnemo = "ADC"; operand_count = 2; operand1 = Some "A"; operand2 = Some "C"; size = 1; cycles = "4"; }
| 0x2b -> { mnemo = "DEC"; operand_count = 1; operand1 = Some "HL"; operand2 = None; size = 1; cycles = "8"; }
| 0x2c -> { mnemo = "INC"; operand_count = 1; operand1 = Some "L"; operand2 = None; size = 1; cycles = "4"; }
| 0x2a -> { mnemo = "LD"; operand_count = 2; operand1 = Some "A"; operand2 = Some "(HL+)"; size = 1; cycles = "8"; }
| 0x2f -> { mnemo = "CPL"; operand_count = 0; operand1 = None; operand2 = None; size = 1; cycles = "4"; }
| 0xff -> { mnemo = "RST"; operand_count = 1; operand1 = Some "38H"; operand2 = None; size = 1; cycles = "16"; }
| 0x2d -> { mnemo = "DEC"; operand_count = 1; operand1 = Some "L"; operand2 = None; size = 1; cycles = "4"; }
| 0x2e -> { mnemo = "LD"; operand_count = 2; operand1 = Some "L"; operand2 = Some "d8"; size = 2; cycles = "8"; }
| 0x5c -> { mnemo = "LD"; operand_count = 2; operand1 = Some "E"; operand2 = Some "H"; size = 1; cycles = "4"; }
| 0x5b -> { mnemo = "LD"; operand_count = 2; operand1 = Some "E"; operand2 = Some "E"; size = 1; cycles = "4"; }
| 0x09 -> { mnemo = "ADD"; operand_count = 2; operand1 = Some "HL"; operand2 = Some "BC"; size = 1; cycles = "8"; }
| 0x08 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "(a16)"; operand2 = Some "SP"; size = 3; cycles = "20"; }
| 0x5f -> { mnemo = "LD"; operand_count = 2; operand1 = Some "E"; operand2 = Some "A"; size = 1; cycles = "4"; }
| 0x5e -> { mnemo = "LD"; operand_count = 2; operand1 = Some "E"; operand2 = Some "(HL)"; size = 1; cycles = "8"; }
| 0x5d -> { mnemo = "LD"; operand_count = 2; operand1 = Some "E"; operand2 = Some "L"; size = 1; cycles = "4"; }
| 0x03 -> { mnemo = "INC"; operand_count = 1; operand1 = Some "BC"; operand2 = None; size = 1; cycles = "8"; }
| 0x02 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "(BC)"; operand2 = Some "A"; size = 1; cycles = "8"; }
| 0x01 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "BC"; operand2 = Some "d16"; size = 3; cycles = "12"; }
| 0x00 -> { mnemo = "NOP"; operand_count = 0; operand1 = None; operand2 = None; size = 1; cycles = "4"; }
| 0x07 -> { mnemo = "RLCA"; operand_count = 0; operand1 = None; operand2 = None; size = 1; cycles = "4"; }
| 0x06 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "B"; operand2 = Some "d8"; size = 2; cycles = "8"; }
| 0x05 -> { mnemo = "DEC"; operand_count = 1; operand1 = Some "B"; operand2 = None; size = 1; cycles = "4"; }
| 0x04 -> { mnemo = "INC"; operand_count = 1; operand1 = Some "B"; operand2 = None; size = 1; cycles = "4"; }
| 0x40 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "B"; operand2 = Some "B"; size = 1; cycles = "4"; }
| 0x41 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "B"; operand2 = Some "C"; size = 1; cycles = "4"; }
| 0x42 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "B"; operand2 = Some "D"; size = 1; cycles = "4"; }
| 0x43 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "B"; operand2 = Some "E"; size = 1; cycles = "4"; }
| 0x44 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "B"; operand2 = Some "H"; size = 1; cycles = "4"; }
| 0x45 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "B"; operand2 = Some "L"; size = 1; cycles = "4"; }
| 0x46 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "B"; operand2 = Some "(HL)"; size = 1; cycles = "8"; }
| 0x47 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "B"; operand2 = Some "A"; size = 1; cycles = "4"; }
| 0x48 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "C"; operand2 = Some "B"; size = 1; cycles = "4"; }
| 0x49 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "C"; operand2 = Some "C"; size = 1; cycles = "4"; }
| 0xe7 -> { mnemo = "RST"; operand_count = 1; operand1 = Some "20H"; operand2 = None; size = 1; cycles = "16"; }
| 0xaf -> { mnemo = "XOR"; operand_count = 1; operand1 = Some "A"; operand2 = None; size = 1; cycles = "4"; }
| 0xae -> { mnemo = "XOR"; operand_count = 1; operand1 = Some "(HL)"; operand2 = None; size = 1; cycles = "8"; }
| 0xad -> { mnemo = "XOR"; operand_count = 1; operand1 = Some "L"; operand2 = None; size = 1; cycles = "4"; }
| 0xac -> { mnemo = "XOR"; operand_count = 1; operand1 = Some "H"; operand2 = None; size = 1; cycles = "4"; }
| 0xab -> { mnemo = "XOR"; operand_count = 1; operand1 = Some "E"; operand2 = None; size = 1; cycles = "4"; }
| 0xaa -> { mnemo = "XOR"; operand_count = 1; operand1 = Some "D"; operand2 = None; size = 1; cycles = "4"; }
| 0x63 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "H"; operand2 = Some "E"; size = 1; cycles = "4"; }
| 0xe8 -> { mnemo = "ADD"; operand_count = 2; operand1 = Some "SP"; operand2 = Some "r8"; size = 2; cycles = "16"; }
| 0xef -> { mnemo = "RST"; operand_count = 1; operand1 = Some "28H"; operand2 = None; size = 1; cycles = "16"; }
| 0x4a -> { mnemo = "LD"; operand_count = 2; operand1 = Some "C"; operand2 = Some "D"; size = 1; cycles = "4"; }
| 0x4b -> { mnemo = "LD"; operand_count = 2; operand1 = Some "C"; operand2 = Some "E"; size = 1; cycles = "4"; }
| 0x4c -> { mnemo = "LD"; operand_count = 2; operand1 = Some "C"; operand2 = Some "H"; size = 1; cycles = "4"; }
| 0x4d -> { mnemo = "LD"; operand_count = 2; operand1 = Some "C"; operand2 = Some "L"; size = 1; cycles = "4"; }
| 0x4e -> { mnemo = "LD"; operand_count = 2; operand1 = Some "C"; operand2 = Some "(HL)"; size = 1; cycles = "8"; }
| 0x4f -> { mnemo = "LD"; operand_count = 2; operand1 = Some "C"; operand2 = Some "A"; size = 1; cycles = "4"; }
| 0x53 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "D"; operand2 = Some "E"; size = 1; cycles = "4"; }
| 0x52 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "D"; operand2 = Some "D"; size = 1; cycles = "4"; }
| 0x51 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "D"; operand2 = Some "C"; size = 1; cycles = "4"; }
| 0x50 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "D"; operand2 = Some "B"; size = 1; cycles = "4"; }
| 0x57 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "D"; operand2 = Some "A"; size = 1; cycles = "4"; }
| 0x56 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "D"; operand2 = Some "(HL)"; size = 1; cycles = "8"; }
| 0x55 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "D"; operand2 = Some "L"; size = 1; cycles = "4"; }
| 0x54 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "D"; operand2 = Some "H"; size = 1; cycles = "4"; }
| 0x0c -> { mnemo = "INC"; operand_count = 1; operand1 = Some "C"; operand2 = None; size = 1; cycles = "4"; }
| 0x0b -> { mnemo = "DEC"; operand_count = 1; operand1 = Some "BC"; operand2 = None; size = 1; cycles = "8"; }
| 0x0a -> { mnemo = "LD"; operand_count = 2; operand1 = Some "A"; operand2 = Some "(BC)"; size = 1; cycles = "8"; }
| 0x58 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "E"; operand2 = Some "B"; size = 1; cycles = "4"; }
| 0x0f -> { mnemo = "RRCA"; operand_count = 0; operand1 = None; operand2 = None; size = 1; cycles = "4"; }
| 0x0e -> { mnemo = "LD"; operand_count = 2; operand1 = Some "C"; operand2 = Some "d8"; size = 2; cycles = "8"; }
| 0x0d -> { mnemo = "DEC"; operand_count = 1; operand1 = Some "C"; operand2 = None; size = 1; cycles = "4"; }
| 0xf5 -> { mnemo = "PUSH"; operand_count = 1; operand1 = Some "AF"; operand2 = None; size = 1; cycles = "16"; }
| 0xfe -> { mnemo = "CP"; operand_count = 1; operand1 = Some "d8"; operand2 = None; size = 2; cycles = "8"; }
| 0xdf -> { mnemo = "RST"; operand_count = 1; operand1 = Some "18H"; operand2 = None; size = 1; cycles = "16"; }
| 0xf7 -> { mnemo = "RST"; operand_count = 1; operand1 = Some "30H"; operand2 = None; size = 1; cycles = "16"; }
| 0xa9 -> { mnemo = "XOR"; operand_count = 1; operand1 = Some "C"; operand2 = None; size = 1; cycles = "4"; }
| 0xa8 -> { mnemo = "XOR"; operand_count = 1; operand1 = Some "B"; operand2 = None; size = 1; cycles = "4"; }
| 0xa7 -> { mnemo = "AND"; operand_count = 1; operand1 = Some "A"; operand2 = None; size = 1; cycles = "4"; }
| 0xa6 -> { mnemo = "AND"; operand_count = 1; operand1 = Some "(HL)"; operand2 = None; size = 1; cycles = "8"; }
| 0xa5 -> { mnemo = "AND"; operand_count = 1; operand1 = Some "L"; operand2 = None; size = 1; cycles = "4"; }
| 0xa4 -> { mnemo = "AND"; operand_count = 1; operand1 = Some "H"; operand2 = None; size = 1; cycles = "4"; }
| 0xa3 -> { mnemo = "AND"; operand_count = 1; operand1 = Some "E"; operand2 = None; size = 1; cycles = "4"; }
| 0xa2 -> { mnemo = "AND"; operand_count = 1; operand1 = Some "D"; operand2 = None; size = 1; cycles = "4"; }
| 0xa1 -> { mnemo = "AND"; operand_count = 1; operand1 = Some "C"; operand2 = None; size = 1; cycles = "4"; }
| 0xa0 -> { mnemo = "AND"; operand_count = 1; operand1 = Some "B"; operand2 = None; size = 1; cycles = "4"; }
| 0xf6 -> { mnemo = "OR"; operand_count = 1; operand1 = Some "d8"; operand2 = None; size = 2; cycles = "8"; }
| 0x7a -> { mnemo = "LD"; operand_count = 2; operand1 = Some "A"; operand2 = Some "D"; size = 1; cycles = "4"; }
| 0xf2 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "A"; operand2 = Some "(C)"; size = 1; cycles = "8"; }
| 0x7c -> { mnemo = "LD"; operand_count = 2; operand1 = Some "A"; operand2 = Some "H"; size = 1; cycles = "4"; }
| 0x7b -> { mnemo = "LD"; operand_count = 2; operand1 = Some "A"; operand2 = Some "E"; size = 1; cycles = "4"; }
| 0x7e -> { mnemo = "LD"; operand_count = 2; operand1 = Some "A"; operand2 = Some "(HL)"; size = 1; cycles = "8"; }
| 0x5a -> { mnemo = "LD"; operand_count = 2; operand1 = Some "E"; operand2 = Some "D"; size = 1; cycles = "4"; }
| 0x7f -> { mnemo = "LD"; operand_count = 2; operand1 = Some "A"; operand2 = Some "A"; size = 1; cycles = "4"; }
| 0xf0 -> { mnemo = "LDH"; operand_count = 2; operand1 = Some "A"; operand2 = Some "(a8)"; size = 2; cycles = "12"; }
| 0x68 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "L"; operand2 = Some "B"; size = 1; cycles = "4"; }
| 0x69 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "L"; operand2 = Some "C"; size = 1; cycles = "4"; }
| 0x66 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "H"; operand2 = Some "(HL)"; size = 1; cycles = "8"; }
| 0x67 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "H"; operand2 = Some "A"; size = 1; cycles = "4"; }
| 0x64 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "H"; operand2 = Some "H"; size = 1; cycles = "4"; }
| 0x65 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "H"; operand2 = Some "L"; size = 1; cycles = "4"; }
| 0x62 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "H"; operand2 = Some "D"; size = 1; cycles = "4"; }
| 0x59 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "E"; operand2 = Some "C"; size = 1; cycles = "4"; }
| 0x60 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "H"; operand2 = Some "B"; size = 1; cycles = "4"; }
| 0x61 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "H"; operand2 = Some "C"; size = 1; cycles = "4"; }
| 0xce -> { mnemo = "ADC"; operand_count = 2; operand1 = Some "A"; operand2 = Some "d8"; size = 2; cycles = "8"; }
| 0xcd -> { mnemo = "CALL"; operand_count = 1; operand1 = Some "a16"; operand2 = None; size = 3; cycles = "24"; }
| 0xb8 -> { mnemo = "CP"; operand_count = 1; operand1 = Some "B"; operand2 = None; size = 1; cycles = "4"; }
| 0xb9 -> { mnemo = "CP"; operand_count = 1; operand1 = Some "C"; operand2 = None; size = 1; cycles = "4"; }
| 0xca -> { mnemo = "JP"; operand_count = 2; operand1 = Some "Z"; operand2 = Some "a16"; size = 3; cycles = "16/12"; }
| 0xcc -> { mnemo = "CALL"; operand_count = 2; operand1 = Some "Z"; operand2 = Some "a16"; size = 3; cycles = "24/12"; }
| 0xcb -> { mnemo = "PREFIX"; operand_count = 1; operand1 = Some "CB"; operand2 = None; size = 1; cycles = "4"; }
| 0xb2 -> { mnemo = "OR"; operand_count = 1; operand1 = Some "D"; operand2 = None; size = 1; cycles = "4"; }
| 0xb3 -> { mnemo = "OR"; operand_count = 1; operand1 = Some "E"; operand2 = None; size = 1; cycles = "4"; }
| 0xb0 -> { mnemo = "OR"; operand_count = 1; operand1 = Some "B"; operand2 = None; size = 1; cycles = "4"; }
| 0xb1 -> { mnemo = "OR"; operand_count = 1; operand1 = Some "C"; operand2 = None; size = 1; cycles = "4"; }
| 0xb6 -> { mnemo = "OR"; operand_count = 1; operand1 = Some "(HL)"; operand2 = None; size = 1; cycles = "8"; }
| 0xb7 -> { mnemo = "OR"; operand_count = 1; operand1 = Some "A"; operand2 = None; size = 1; cycles = "4"; }
| 0xb4 -> { mnemo = "OR"; operand_count = 1; operand1 = Some "H"; operand2 = None; size = 1; cycles = "4"; }
| 0xb5 -> { mnemo = "OR"; operand_count = 1; operand1 = Some "L"; operand2 = None; size = 1; cycles = "4"; }
| 0xe5 -> { mnemo = "PUSH"; operand_count = 1; operand1 = Some "HL"; operand2 = None; size = 1; cycles = "16"; }
| 0xd7 -> { mnemo = "RST"; operand_count = 1; operand1 = Some "10H"; operand2 = None; size = 1; cycles = "16"; }
| 0x6f -> { mnemo = "LD"; operand_count = 2; operand1 = Some "L"; operand2 = Some "A"; size = 1; cycles = "4"; }
| 0x6d -> { mnemo = "LD"; operand_count = 2; operand1 = Some "L"; operand2 = Some "L"; size = 1; cycles = "4"; }
| 0x6e -> { mnemo = "LD"; operand_count = 2; operand1 = Some "L"; operand2 = Some "(HL)"; size = 1; cycles = "8"; }
| 0x6b -> { mnemo = "LD"; operand_count = 2; operand1 = Some "L"; operand2 = Some "E"; size = 1; cycles = "4"; }
| 0x6c -> { mnemo = "LD"; operand_count = 2; operand1 = Some "L"; operand2 = Some "H"; size = 1; cycles = "4"; }
| 0x6a -> { mnemo = "LD"; operand_count = 2; operand1 = Some "L"; operand2 = Some "D"; size = 1; cycles = "4"; }
| 0x79 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "A"; operand2 = Some "C"; size = 1; cycles = "4"; }
| 0x78 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "A"; operand2 = Some "B"; size = 1; cycles = "4"; }
| 0x71 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "(HL)"; operand2 = Some "C"; size = 1; cycles = "8"; }
| 0x70 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "(HL)"; operand2 = Some "B"; size = 1; cycles = "8"; }
| 0x73 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "(HL)"; operand2 = Some "E"; size = 1; cycles = "8"; }
| 0x72 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "(HL)"; operand2 = Some "D"; size = 1; cycles = "8"; }
| 0x75 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "(HL)"; operand2 = Some "L"; size = 1; cycles = "8"; }
| 0x74 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "(HL)"; operand2 = Some "H"; size = 1; cycles = "8"; }
| 0x77 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "(HL)"; operand2 = Some "A"; size = 1; cycles = "8"; }
| 0x76 -> { mnemo = "HALT"; operand_count = 0; operand1 = None; operand2 = None; size = 1; cycles = "4"; }
| 0xc5 -> { mnemo = "PUSH"; operand_count = 1; operand1 = Some "BC"; operand2 = None; size = 1; cycles = "16"; }
| 0xc4 -> { mnemo = "CALL"; operand_count = 2; operand1 = Some "NZ"; operand2 = Some "a16"; size = 3; cycles = "24/12"; }
| 0xc7 -> { mnemo = "RST"; operand_count = 1; operand1 = Some "00H"; operand2 = None; size = 1; cycles = "16"; }
| 0xc6 -> { mnemo = "ADD"; operand_count = 2; operand1 = Some "A"; operand2 = Some "d8"; size = 2; cycles = "8"; }
| 0xc1 -> { mnemo = "POP"; operand_count = 1; operand1 = Some "BC"; operand2 = None; size = 1; cycles = "12"; }
| 0x8b -> { mnemo = "ADC"; operand_count = 2; operand1 = Some "A"; operand2 = Some "E"; size = 1; cycles = "4"; }
| 0xc3 -> { mnemo = "JP"; operand_count = 1; operand1 = Some "a16"; operand2 = None; size = 3; cycles = "16"; }
| 0xc2 -> { mnemo = "JP"; operand_count = 2; operand1 = Some "NZ"; operand2 = Some "a16"; size = 3; cycles = "16/12"; }
| 0xbb -> { mnemo = "CP"; operand_count = 1; operand1 = Some "E"; operand2 = None; size = 1; cycles = "4"; }
| 0xbc -> { mnemo = "CP"; operand_count = 1; operand1 = Some "H"; operand2 = None; size = 1; cycles = "4"; }
| 0x8c -> { mnemo = "ADC"; operand_count = 2; operand1 = Some "A"; operand2 = Some "H"; size = 1; cycles = "4"; }
| 0xbf -> { mnemo = "CP"; operand_count = 1; operand1 = Some "A"; operand2 = None; size = 1; cycles = "4"; }
| 0xc8 -> { mnemo = "RET"; operand_count = 1; operand1 = Some "Z"; operand2 = None; size = 1; cycles = "20/8"; }
| 0xbd -> { mnemo = "CP"; operand_count = 1; operand1 = Some "L"; operand2 = None; size = 1; cycles = "4"; }
| 0xbe -> { mnemo = "CP"; operand_count = 1; operand1 = Some "(HL)"; operand2 = None; size = 1; cycles = "8"; }
| 0xf1 -> { mnemo = "POP"; operand_count = 1; operand1 = Some "AF"; operand2 = None; size = 1; cycles = "12"; }
| 0xee -> { mnemo = "XOR"; operand_count = 1; operand1 = Some "d8"; operand2 = None; size = 2; cycles = "8"; }
| 0xea -> { mnemo = "LD"; operand_count = 2; operand1 = Some "(a16)"; operand2 = Some "A"; size = 3; cycles = "16"; }
| 0xf8 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "HL"; operand2 = Some "SP+r8"; size = 2; cycles = "12"; }
| 0xc9 -> { mnemo = "RET"; operand_count = 0; operand1 = None; operand2 = None; size = 1; cycles = "16"; }
| 0xf3 -> { mnemo = "DI"; operand_count = 0; operand1 = None; operand2 = None; size = 1; cycles = "4"; }
| 0xd0 -> { mnemo = "RET"; operand_count = 1; operand1 = Some "NC"; operand2 = None; size = 1; cycles = "20/8"; }
| 0x9f -> { mnemo = "SBC"; operand_count = 2; operand1 = Some "A"; operand2 = Some "A"; size = 1; cycles = "4"; }
| 0x9e -> { mnemo = "SBC"; operand_count = 2; operand1 = Some "A"; operand2 = Some "(HL)"; size = 1; cycles = "8"; }
| 0x9d -> { mnemo = "SBC"; operand_count = 2; operand1 = Some "A"; operand2 = Some "L"; size = 1; cycles = "4"; }
| 0x9c -> { mnemo = "SBC"; operand_count = 2; operand1 = Some "A"; operand2 = Some "H"; size = 1; cycles = "4"; }
| 0x9b -> { mnemo = "SBC"; operand_count = 2; operand1 = Some "A"; operand2 = Some "E"; size = 1; cycles = "4"; }
| 0x9a -> { mnemo = "SBC"; operand_count = 2; operand1 = Some "A"; operand2 = Some "D"; size = 1; cycles = "4"; }
| 0xd8 -> { mnemo = "RET"; operand_count = 1; operand1 = Some "C"; operand2 = None; size = 1; cycles = "20/8"; }
| 0xd9 -> { mnemo = "RETI"; operand_count = 0; operand1 = None; operand2 = None; size = 1; cycles = "16"; }
| 0xdA -> { mnemo = "JP"; operand_count = 2; operand1 = Some "C"; operand2 = Some "a16"; size = 3; cycles = "16/12"; }
| 0xde -> { mnemo = "SBC"; operand_count = 2; operand1 = Some "A"; operand2 = Some "d8"; size = 2; cycles = "8"; }
| 0x84 -> { mnemo = "ADD"; operand_count = 2; operand1 = Some "A"; operand2 = Some "H"; size = 1; cycles = "4"; }
| 0x85 -> { mnemo = "ADD"; operand_count = 2; operand1 = Some "A"; operand2 = Some "L"; size = 1; cycles = "4"; }
| 0x86 -> { mnemo = "ADD"; operand_count = 2; operand1 = Some "A"; operand2 = Some "(HL)"; size = 1; cycles = "8"; }
| 0x87 -> { mnemo = "ADD"; operand_count = 2; operand1 = Some "A"; operand2 = Some "A"; size = 1; cycles = "4"; }
| 0x80 -> { mnemo = "ADD"; operand_count = 2; operand1 = Some "A"; operand2 = Some "B"; size = 1; cycles = "4"; }
| 0x81 -> { mnemo = "ADD"; operand_count = 2; operand1 = Some "A"; operand2 = Some "C"; size = 1; cycles = "4"; }
| 0x82 -> { mnemo = "ADD"; operand_count = 2; operand1 = Some "A"; operand2 = Some "D"; size = 1; cycles = "4"; }
| 0x83 -> { mnemo = "ADD"; operand_count = 2; operand1 = Some "A"; operand2 = Some "E"; size = 1; cycles = "4"; }
| 0x1f -> { mnemo = "RRA"; operand_count = 0; operand1 = None; operand2 = None; size = 1; cycles = "4"; }
| 0x1e -> { mnemo = "LD"; operand_count = 2; operand1 = Some "E"; operand2 = Some "d8"; size = 2; cycles = "8"; }
| 0x1d -> { mnemo = "DEC"; operand_count = 1; operand1 = Some "E"; operand2 = None; size = 1; cycles = "4"; }
| 0x1c -> { mnemo = "INC"; operand_count = 1; operand1 = Some "E"; operand2 = None; size = 1; cycles = "4"; }
| 0x1b -> { mnemo = "DEC"; operand_count = 1; operand1 = Some "DE"; operand2 = None; size = 1; cycles = "8"; }
| 0x1a -> { mnemo = "LD"; operand_count = 2; operand1 = Some "A"; operand2 = Some "(DE)"; size = 1; cycles = "8"; }
| 0xd1 -> { mnemo = "POP"; operand_count = 1; operand1 = Some "DE"; operand2 = None; size = 1; cycles = "12"; }
| 0xd2 -> { mnemo = "JP"; operand_count = 2; operand1 = Some "NC"; operand2 = Some "a16"; size = 3; cycles = "16/12"; }
| 0xd4 -> { mnemo = "CALL"; operand_count = 2; operand1 = Some "NC"; operand2 = Some "a16"; size = 3; cycles = "24/12"; }
| 0xd5 -> { mnemo = "PUSH"; operand_count = 1; operand1 = Some "DE"; operand2 = None; size = 1; cycles = "16"; }
| 0xd6 -> { mnemo = "SUB"; operand_count = 1; operand1 = Some "d8"; operand2 = None; size = 2; cycles = "8"; }
| 0x8d -> { mnemo = "ADC"; operand_count = 2; operand1 = Some "A"; operand2 = Some "L"; size = 1; cycles = "4"; }
| 0x8e -> { mnemo = "ADC"; operand_count = 2; operand1 = Some "A"; operand2 = Some "(HL)"; size = 1; cycles = "8"; }
| 0x8f -> { mnemo = "ADC"; operand_count = 2; operand1 = Some "A"; operand2 = Some "A"; size = 1; cycles = "4"; }
| 0xe0 -> { mnemo = "LDH"; operand_count = 2; operand1 = Some "(a8)"; operand2 = Some "A"; size = 2; cycles = "12"; }
| 0xe9 -> { mnemo = "JP"; operand_count = 1; operand1 = Some "(HL)"; operand2 = None; size = 1; cycles = "4"; }
| 0x8a -> { mnemo = "ADC"; operand_count = 2; operand1 = Some "A"; operand2 = Some "D"; size = 1; cycles = "4"; }
| 0x19 -> { mnemo = "ADD"; operand_count = 2; operand1 = Some "HL"; operand2 = Some "DE"; size = 1; cycles = "8"; }
| 0x18 -> { mnemo = "JR"; operand_count = 1; operand1 = Some "r8"; operand2 = None; size = 2; cycles = "12"; }
| 0x17 -> { mnemo = "RLA"; operand_count = 0; operand1 = None; operand2 = None; size = 1; cycles = "4"; }
| 0x16 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "D"; operand2 = Some "d8"; size = 2; cycles = "8"; }
| 0x15 -> { mnemo = "DEC"; operand_count = 1; operand1 = Some "D"; operand2 = None; size = 1; cycles = "4"; }
| 0x14 -> { mnemo = "INC"; operand_count = 1; operand1 = Some "D"; operand2 = None; size = 1; cycles = "4"; }
| 0x13 -> { mnemo = "INC"; operand_count = 1; operand1 = Some "DE"; operand2 = None; size = 1; cycles = "8"; }
| 0x12 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "(DE)"; operand2 = Some "A"; size = 1; cycles = "8"; }
| 0x11 -> { mnemo = "LD"; operand_count = 2; operand1 = Some "DE"; operand2 = Some "d16"; size = 3; cycles = "12"; }
| 0x10 -> { mnemo = "STOP"; operand_count = 1; operand1 = Some "0"; operand2 = None; size = 2; cycles = "4"; }
| 0x97 -> { mnemo = "SUB"; operand_count = 1; operand1 = Some "A"; operand2 = None; size = 1; cycles = "4"; }
| 0x96 -> { mnemo = "SUB"; operand_count = 1; operand1 = Some "(HL)"; operand2 = None; size = 1; cycles = "8"; }
| 0x95 -> { mnemo = "SUB"; operand_count = 1; operand1 = Some "L"; operand2 = None; size = 1; cycles = "4"; }
| 0x94 -> { mnemo = "SUB"; operand_count = 1; operand1 = Some "H"; operand2 = None; size = 1; cycles = "4"; }
| 0x93 -> { mnemo = "SUB"; operand_count = 1; operand1 = Some "E"; operand2 = None; size = 1; cycles = "4"; }
| 0x92 -> { mnemo = "SUB"; operand_count = 1; operand1 = Some "D"; operand2 = None; size = 1; cycles = "4"; }
| 0x91 -> { mnemo = "SUB"; operand_count = 1; operand1 = Some "C"; operand2 = None; size = 1; cycles = "4"; }
| 0x90 -> { mnemo = "SUB"; operand_count = 1; operand1 = Some "B"; operand2 = None; size = 1; cycles = "4"; }
| 0x99 -> { mnemo = "SBC"; operand_count = 2; operand1 = Some "A"; operand2 = Some "C"; size = 1; cycles = "4"; }
| 0x98 -> { mnemo = "SBC"; operand_count = 2; operand1 = Some "A"; operand2 = Some "B"; size = 1; cycles = "4"; }
| _ -> raise Not_found

let get_ext_opcode_repr = function
| 0x3e -> { mnemo = "SRL"; operand_count = 1; operand1 = Some "(HL)"; operand2 = None; size = 2; cycles = "16"; }
| 0x3d -> { mnemo = "SRL"; operand_count = 1; operand1 = Some "L"; operand2 = None; size = 2; cycles = "8"; }
| 0xe4 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "4"; operand2 = Some "H"; size = 2; cycles = "8"; }
| 0x3f -> { mnemo = "SRL"; operand_count = 1; operand1 = Some "A"; operand2 = None; size = 2; cycles = "8"; }
| 0x3a -> { mnemo = "SRL"; operand_count = 1; operand1 = Some "D"; operand2 = None; size = 2; cycles = "8"; }
| 0x3c -> { mnemo = "SRL"; operand_count = 1; operand1 = Some "H"; operand2 = None; size = 2; cycles = "8"; }
| 0x3b -> { mnemo = "SRL"; operand_count = 1; operand1 = Some "E"; operand2 = None; size = 2; cycles = "8"; }
| 0xff -> { mnemo = "SET"; operand_count = 2; operand1 = Some "7"; operand2 = Some "A"; size = 2; cycles = "8"; }
| 0xfa -> { mnemo = "SET"; operand_count = 2; operand1 = Some "7"; operand2 = Some "D"; size = 2; cycles = "8"; }
| 0xda -> { mnemo = "SET"; operand_count = 2; operand1 = Some "3"; operand2 = Some "D"; size = 2; cycles = "8"; }
| 0xec -> { mnemo = "SET"; operand_count = 2; operand1 = Some "5"; operand2 = Some "H"; size = 2; cycles = "8"; }
| 0x28 -> { mnemo = "SRA"; operand_count = 1; operand1 = Some "B"; operand2 = None; size = 2; cycles = "8"; }
| 0x29 -> { mnemo = "SRA"; operand_count = 1; operand1 = Some "C"; operand2 = None; size = 2; cycles = "8"; }
| 0xcf -> { mnemo = "SET"; operand_count = 2; operand1 = Some "1"; operand2 = Some "A"; size = 2; cycles = "8"; }
| 0xf8 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "7"; operand2 = Some "B"; size = 2; cycles = "8"; }
| 0xeb -> { mnemo = "SET"; operand_count = 2; operand1 = Some "5"; operand2 = Some "E"; size = 2; cycles = "8"; }
| 0x22 -> { mnemo = "SLA"; operand_count = 1; operand1 = Some "D"; operand2 = None; size = 2; cycles = "8"; }
| 0x23 -> { mnemo = "SLA"; operand_count = 1; operand1 = Some "E"; operand2 = None; size = 2; cycles = "8"; }
| 0x20 -> { mnemo = "SLA"; operand_count = 1; operand1 = Some "B"; operand2 = None; size = 2; cycles = "8"; }
| 0x21 -> { mnemo = "SLA"; operand_count = 1; operand1 = Some "C"; operand2 = None; size = 2; cycles = "8"; }
| 0x26 -> { mnemo = "SLA"; operand_count = 1; operand1 = Some "(HL)"; operand2 = None; size = 2; cycles = "16"; }
| 0x27 -> { mnemo = "SLA"; operand_count = 1; operand1 = Some "A"; operand2 = None; size = 2; cycles = "8"; }
| 0x24 -> { mnemo = "SLA"; operand_count = 1; operand1 = Some "H"; operand2 = None; size = 2; cycles = "8"; }
| 0x25 -> { mnemo = "SLA"; operand_count = 1; operand1 = Some "L"; operand2 = None; size = 2; cycles = "8"; }
| 0xba -> { mnemo = "RES"; operand_count = 2; operand1 = Some "7"; operand2 = Some "D"; size = 2; cycles = "8"; }
| 0xef -> { mnemo = "SET"; operand_count = 2; operand1 = Some "5"; operand2 = Some "A"; size = 2; cycles = "8"; }
| 0xe2 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "4"; operand2 = Some "D"; size = 2; cycles = "8"; }
| 0xee -> { mnemo = "SET"; operand_count = 2; operand1 = Some "5"; operand2 = Some "(HL)"; size = 2; cycles = "16"; }
| 0xed -> { mnemo = "SET"; operand_count = 2; operand1 = Some "5"; operand2 = Some "L"; size = 2; cycles = "8"; }
| 0x7d -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "7"; operand2 = Some "L"; size = 2; cycles = "8"; }
| 0x35 -> { mnemo = "SWAP"; operand_count = 1; operand1 = Some "L"; operand2 = None; size = 2; cycles = "8"; }
| 0x34 -> { mnemo = "SWAP"; operand_count = 1; operand1 = Some "H"; operand2 = None; size = 2; cycles = "8"; }
| 0x37 -> { mnemo = "SWAP"; operand_count = 1; operand1 = Some "A"; operand2 = None; size = 2; cycles = "8"; }
| 0x36 -> { mnemo = "SWAP"; operand_count = 1; operand1 = Some "(HL)"; operand2 = None; size = 2; cycles = "16"; }
| 0x31 -> { mnemo = "SWAP"; operand_count = 1; operand1 = Some "C"; operand2 = None; size = 2; cycles = "8"; }
| 0x30 -> { mnemo = "SWAP"; operand_count = 1; operand1 = Some "B"; operand2 = None; size = 2; cycles = "8"; }
| 0x33 -> { mnemo = "SWAP"; operand_count = 1; operand1 = Some "E"; operand2 = None; size = 2; cycles = "8"; }
| 0x32 -> { mnemo = "SWAP"; operand_count = 1; operand1 = Some "D"; operand2 = None; size = 2; cycles = "8"; }
| 0x39 -> { mnemo = "SRL"; operand_count = 1; operand1 = Some "C"; operand2 = None; size = 2; cycles = "8"; }
| 0x38 -> { mnemo = "SRL"; operand_count = 1; operand1 = Some "B"; operand2 = None; size = 2; cycles = "8"; }
| 0xc0 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "0"; operand2 = Some "B"; size = 2; cycles = "8"; }
| 0xe1 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "4"; operand2 = Some "C"; size = 2; cycles = "8"; }
| 0xfe -> { mnemo = "SET"; operand_count = 2; operand1 = Some "7"; operand2 = Some "(HL)"; size = 2; cycles = "16"; }
| 0x88 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "1"; operand2 = Some "B"; size = 2; cycles = "8"; }
| 0xdd -> { mnemo = "SET"; operand_count = 2; operand1 = Some "3"; operand2 = Some "L"; size = 2; cycles = "8"; }
| 0x89 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "1"; operand2 = Some "C"; size = 2; cycles = "8"; }
| 0x2b -> { mnemo = "SRA"; operand_count = 1; operand1 = Some "E"; operand2 = None; size = 2; cycles = "8"; }
| 0x2c -> { mnemo = "SRA"; operand_count = 1; operand1 = Some "H"; operand2 = None; size = 2; cycles = "8"; }
| 0xfd -> { mnemo = "SET"; operand_count = 2; operand1 = Some "7"; operand2 = Some "L"; size = 2; cycles = "8"; }
| 0x2a -> { mnemo = "SRA"; operand_count = 1; operand1 = Some "D"; operand2 = None; size = 2; cycles = "8"; }
| 0x2f -> { mnemo = "SRA"; operand_count = 1; operand1 = Some "A"; operand2 = None; size = 2; cycles = "8"; }
| 0xfc -> { mnemo = "SET"; operand_count = 2; operand1 = Some "7"; operand2 = Some "H"; size = 2; cycles = "8"; }
| 0x2d -> { mnemo = "SRA"; operand_count = 1; operand1 = Some "L"; operand2 = None; size = 2; cycles = "8"; }
| 0x2e -> { mnemo = "SRA"; operand_count = 1; operand1 = Some "(HL)"; operand2 = None; size = 2; cycles = "16"; }
| 0x5c -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "3"; operand2 = Some "H"; size = 2; cycles = "8"; }
| 0x5b -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "3"; operand2 = Some "E"; size = 2; cycles = "8"; }
| 0x09 -> { mnemo = "RRC"; operand_count = 1; operand1 = Some "C"; operand2 = None; size = 2; cycles = "8"; }
| 0x08 -> { mnemo = "RRC"; operand_count = 1; operand1 = Some "B"; operand2 = None; size = 2; cycles = "8"; }
| 0x5f -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "3"; operand2 = Some "A"; size = 2; cycles = "8"; }
| 0x5e -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "3"; operand2 = Some "(HL)"; size = 2; cycles = "16"; }
| 0x5d -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "3"; operand2 = Some "L"; size = 2; cycles = "8"; }
| 0x03 -> { mnemo = "RLC"; operand_count = 1; operand1 = Some "E"; operand2 = None; size = 2; cycles = "8"; }
| 0x02 -> { mnemo = "RLC"; operand_count = 1; operand1 = Some "D"; operand2 = None; size = 2; cycles = "8"; }
| 0x01 -> { mnemo = "RLC"; operand_count = 1; operand1 = Some "C"; operand2 = None; size = 2; cycles = "8"; }
| 0x00 -> { mnemo = "RLC"; operand_count = 1; operand1 = Some "B"; operand2 = None; size = 2; cycles = "8"; }
| 0x07 -> { mnemo = "RLC"; operand_count = 1; operand1 = Some "A"; operand2 = None; size = 2; cycles = "8"; }
| 0x06 -> { mnemo = "RLC"; operand_count = 1; operand1 = Some "(HL)"; operand2 = None; size = 2; cycles = "16"; }
| 0x05 -> { mnemo = "RLC"; operand_count = 1; operand1 = Some "L"; operand2 = None; size = 2; cycles = "8"; }
| 0x04 -> { mnemo = "RLC"; operand_count = 1; operand1 = Some "H"; operand2 = None; size = 2; cycles = "8"; }
| 0x40 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "0"; operand2 = Some "B"; size = 2; cycles = "8"; }
| 0x41 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "0"; operand2 = Some "C"; size = 2; cycles = "8"; }
| 0x42 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "0"; operand2 = Some "D"; size = 2; cycles = "8"; }
| 0x43 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "0"; operand2 = Some "E"; size = 2; cycles = "8"; }
| 0x44 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "0"; operand2 = Some "H"; size = 2; cycles = "8"; }
| 0x45 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "0"; operand2 = Some "L"; size = 2; cycles = "8"; }
| 0x46 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "0"; operand2 = Some "(HL)"; size = 2; cycles = "16"; }
| 0x47 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "0"; operand2 = Some "A"; size = 2; cycles = "8"; }
| 0x48 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "1"; operand2 = Some "B"; size = 2; cycles = "8"; }
| 0x49 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "1"; operand2 = Some "C"; size = 2; cycles = "8"; }
| 0xe5 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "4"; operand2 = Some "L"; size = 2; cycles = "8"; }
| 0xaf -> { mnemo = "RES"; operand_count = 2; operand1 = Some "5"; operand2 = Some "A"; size = 2; cycles = "8"; }
| 0xae -> { mnemo = "RES"; operand_count = 2; operand1 = Some "5"; operand2 = Some "(HL)"; size = 2; cycles = "16"; }
| 0xad -> { mnemo = "RES"; operand_count = 2; operand1 = Some "5"; operand2 = Some "L"; size = 2; cycles = "8"; }
| 0xac -> { mnemo = "RES"; operand_count = 2; operand1 = Some "5"; operand2 = Some "H"; size = 2; cycles = "8"; }
| 0xab -> { mnemo = "RES"; operand_count = 2; operand1 = Some "5"; operand2 = Some "E"; size = 2; cycles = "8"; }
| 0xaa -> { mnemo = "RES"; operand_count = 2; operand1 = Some "5"; operand2 = Some "D"; size = 2; cycles = "8"; }
| 0x63 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "4"; operand2 = Some "E"; size = 2; cycles = "8"; }
| 0xe6 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "4"; operand2 = Some "(HL)"; size = 2; cycles = "16"; }
| 0xea -> { mnemo = "SET"; operand_count = 2; operand1 = Some "5"; operand2 = Some "D"; size = 2; cycles = "8"; }
| 0x4a -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "1"; operand2 = Some "D"; size = 2; cycles = "8"; }
| 0x4b -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "1"; operand2 = Some "E"; size = 2; cycles = "8"; }
| 0x4c -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "1"; operand2 = Some "H"; size = 2; cycles = "8"; }
| 0x4d -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "1"; operand2 = Some "L"; size = 2; cycles = "8"; }
| 0x4e -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "1"; operand2 = Some "(HL)"; size = 2; cycles = "16"; }
| 0x4f -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "1"; operand2 = Some "A"; size = 2; cycles = "8"; }
| 0x53 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "2"; operand2 = Some "E"; size = 2; cycles = "8"; }
| 0x52 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "2"; operand2 = Some "D"; size = 2; cycles = "8"; }
| 0x51 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "2"; operand2 = Some "C"; size = 2; cycles = "8"; }
| 0x50 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "2"; operand2 = Some "B"; size = 2; cycles = "8"; }
| 0x57 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "2"; operand2 = Some "A"; size = 2; cycles = "8"; }
| 0x56 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "2"; operand2 = Some "(HL)"; size = 2; cycles = "16"; }
| 0x55 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "2"; operand2 = Some "L"; size = 2; cycles = "8"; }
| 0x54 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "2"; operand2 = Some "H"; size = 2; cycles = "8"; }
| 0x0c -> { mnemo = "RRC"; operand_count = 1; operand1 = Some "H"; operand2 = None; size = 2; cycles = "8"; }
| 0x0b -> { mnemo = "RRC"; operand_count = 1; operand1 = Some "E"; operand2 = None; size = 2; cycles = "8"; }
| 0x0a -> { mnemo = "RRC"; operand_count = 1; operand1 = Some "D"; operand2 = None; size = 2; cycles = "8"; }
| 0x58 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "3"; operand2 = Some "B"; size = 2; cycles = "8"; }
| 0x0f -> { mnemo = "RRC"; operand_count = 1; operand1 = Some "A"; operand2 = None; size = 2; cycles = "8"; }
| 0x0e -> { mnemo = "RRC"; operand_count = 1; operand1 = Some "(HL)"; operand2 = None; size = 2; cycles = "16"; }
| 0x0d -> { mnemo = "RRC"; operand_count = 1; operand1 = Some "L"; operand2 = None; size = 2; cycles = "8"; }
| 0xf4 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "6"; operand2 = Some "H"; size = 2; cycles = "8"; }
| 0xfb -> { mnemo = "SET"; operand_count = 2; operand1 = Some "7"; operand2 = Some "E"; size = 2; cycles = "8"; }
| 0xdc -> { mnemo = "SET"; operand_count = 2; operand1 = Some "3"; operand2 = Some "H"; size = 2; cycles = "8"; }
| 0xf9 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "7"; operand2 = Some "C"; size = 2; cycles = "8"; }
| 0xf6 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "6"; operand2 = Some "(HL)"; size = 2; cycles = "16"; }
| 0xa9 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "5"; operand2 = Some "C"; size = 2; cycles = "8"; }
| 0xa8 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "5"; operand2 = Some "B"; size = 2; cycles = "8"; }
| 0xa7 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "4"; operand2 = Some "A"; size = 2; cycles = "8"; }
| 0xa6 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "4"; operand2 = Some "(HL)"; size = 2; cycles = "16"; }
| 0xa5 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "4"; operand2 = Some "L"; size = 2; cycles = "8"; }
| 0xa4 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "4"; operand2 = Some "H"; size = 2; cycles = "8"; }
| 0xa3 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "4"; operand2 = Some "E"; size = 2; cycles = "8"; }
| 0xa2 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "4"; operand2 = Some "D"; size = 2; cycles = "8"; }
| 0xa1 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "4"; operand2 = Some "C"; size = 2; cycles = "8"; }
| 0xa0 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "4"; operand2 = Some "B"; size = 2; cycles = "8"; }
| 0xf5 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "6"; operand2 = Some "L"; size = 2; cycles = "8"; }
| 0x7a -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "7"; operand2 = Some "D"; size = 2; cycles = "8"; }
| 0xf2 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "6"; operand2 = Some "D"; size = 2; cycles = "8"; }
| 0x7c -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "7"; operand2 = Some "H"; size = 2; cycles = "8"; }
| 0x7b -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "7"; operand2 = Some "E"; size = 2; cycles = "8"; }
| 0x7e -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "7"; operand2 = Some "(HL)"; size = 2; cycles = "16"; }
| 0x5a -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "3"; operand2 = Some "D"; size = 2; cycles = "8"; }
| 0x7f -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "7"; operand2 = Some "A"; size = 2; cycles = "8"; }
| 0xf0 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "6"; operand2 = Some "B"; size = 2; cycles = "8"; }
| 0x68 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "5"; operand2 = Some "B"; size = 2; cycles = "8"; }
| 0x69 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "5"; operand2 = Some "C"; size = 2; cycles = "8"; }
| 0x66 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "4"; operand2 = Some "(HL)"; size = 2; cycles = "16"; }
| 0x67 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "4"; operand2 = Some "A"; size = 2; cycles = "8"; }
| 0x64 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "4"; operand2 = Some "H"; size = 2; cycles = "8"; }
| 0x65 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "4"; operand2 = Some "L"; size = 2; cycles = "8"; }
| 0x62 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "4"; operand2 = Some "D"; size = 2; cycles = "8"; }
| 0x59 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "3"; operand2 = Some "C"; size = 2; cycles = "8"; }
| 0x60 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "4"; operand2 = Some "B"; size = 2; cycles = "8"; }
| 0x61 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "4"; operand2 = Some "C"; size = 2; cycles = "8"; }
| 0xce -> { mnemo = "SET"; operand_count = 2; operand1 = Some "1"; operand2 = Some "(HL)"; size = 2; cycles = "16"; }
| 0xcd -> { mnemo = "SET"; operand_count = 2; operand1 = Some "1"; operand2 = Some "L"; size = 2; cycles = "8"; }
| 0xb8 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "7"; operand2 = Some "B"; size = 2; cycles = "8"; }
| 0xb9 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "7"; operand2 = Some "C"; size = 2; cycles = "8"; }
| 0xca -> { mnemo = "SET"; operand_count = 2; operand1 = Some "1"; operand2 = Some "D"; size = 2; cycles = "8"; }
| 0xcc -> { mnemo = "SET"; operand_count = 2; operand1 = Some "1"; operand2 = Some "H"; size = 2; cycles = "8"; }
| 0xcb -> { mnemo = "SET"; operand_count = 2; operand1 = Some "1"; operand2 = Some "E"; size = 2; cycles = "8"; }
| 0xb2 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "6"; operand2 = Some "D"; size = 2; cycles = "8"; }
| 0xb3 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "6"; operand2 = Some "E"; size = 2; cycles = "8"; }
| 0xb0 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "6"; operand2 = Some "B"; size = 2; cycles = "8"; }
| 0xb1 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "6"; operand2 = Some "C"; size = 2; cycles = "8"; }
| 0xb6 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "6"; operand2 = Some "(HL)"; size = 2; cycles = "16"; }
| 0xb7 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "6"; operand2 = Some "A"; size = 2; cycles = "8"; }
| 0xb4 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "6"; operand2 = Some "H"; size = 2; cycles = "8"; }
| 0xb5 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "6"; operand2 = Some "L"; size = 2; cycles = "8"; }
| 0xe3 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "4"; operand2 = Some "E"; size = 2; cycles = "8"; }
| 0xd6 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "2"; operand2 = Some "(HL)"; size = 2; cycles = "16"; }
| 0x6f -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "5"; operand2 = Some "A"; size = 2; cycles = "8"; }
| 0x6d -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "5"; operand2 = Some "L"; size = 2; cycles = "8"; }
| 0x6e -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "5"; operand2 = Some "(HL)"; size = 2; cycles = "16"; }
| 0x6b -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "5"; operand2 = Some "E"; size = 2; cycles = "8"; }
| 0x6c -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "5"; operand2 = Some "H"; size = 2; cycles = "8"; }
| 0x6a -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "5"; operand2 = Some "D"; size = 2; cycles = "8"; }
| 0x79 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "7"; operand2 = Some "C"; size = 2; cycles = "8"; }
| 0x78 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "7"; operand2 = Some "B"; size = 2; cycles = "8"; }
| 0x71 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "6"; operand2 = Some "C"; size = 2; cycles = "8"; }
| 0x70 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "6"; operand2 = Some "B"; size = 2; cycles = "8"; }
| 0x73 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "6"; operand2 = Some "E"; size = 2; cycles = "8"; }
| 0x72 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "6"; operand2 = Some "D"; size = 2; cycles = "8"; }
| 0x75 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "6"; operand2 = Some "L"; size = 2; cycles = "8"; }
| 0x74 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "6"; operand2 = Some "H"; size = 2; cycles = "8"; }
| 0x77 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "6"; operand2 = Some "A"; size = 2; cycles = "8"; }
| 0x76 -> { mnemo = "BIT"; operand_count = 2; operand1 = Some "6"; operand2 = Some "(HL)"; size = 2; cycles = "16"; }
| 0xc5 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "0"; operand2 = Some "L"; size = 2; cycles = "8"; }
| 0xc4 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "0"; operand2 = Some "H"; size = 2; cycles = "8"; }
| 0xc7 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "0"; operand2 = Some "A"; size = 2; cycles = "8"; }
| 0xc6 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "0"; operand2 = Some "(HL)"; size = 2; cycles = "16"; }
| 0xc1 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "0"; operand2 = Some "C"; size = 2; cycles = "8"; }
| 0x8b -> { mnemo = "RES"; operand_count = 2; operand1 = Some "1"; operand2 = Some "E"; size = 2; cycles = "8"; }
| 0xc3 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "0"; operand2 = Some "E"; size = 2; cycles = "8"; }
| 0xc2 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "0"; operand2 = Some "D"; size = 2; cycles = "8"; }
| 0xbb -> { mnemo = "RES"; operand_count = 2; operand1 = Some "7"; operand2 = Some "E"; size = 2; cycles = "8"; }
| 0xbc -> { mnemo = "RES"; operand_count = 2; operand1 = Some "7"; operand2 = Some "H"; size = 2; cycles = "8"; }
| 0x8c -> { mnemo = "RES"; operand_count = 2; operand1 = Some "1"; operand2 = Some "H"; size = 2; cycles = "8"; }
| 0xbf -> { mnemo = "RES"; operand_count = 2; operand1 = Some "7"; operand2 = Some "A"; size = 2; cycles = "8"; }
| 0xc8 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "1"; operand2 = Some "B"; size = 2; cycles = "8"; }
| 0xbd -> { mnemo = "RES"; operand_count = 2; operand1 = Some "7"; operand2 = Some "L"; size = 2; cycles = "8"; }
| 0xbe -> { mnemo = "RES"; operand_count = 2; operand1 = Some "7"; operand2 = Some "(HL)"; size = 2; cycles = "16"; }
| 0xf1 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "6"; operand2 = Some "C"; size = 2; cycles = "8"; }
| 0xe9 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "5"; operand2 = Some "C"; size = 2; cycles = "8"; }
| 0xe8 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "5"; operand2 = Some "B"; size = 2; cycles = "8"; }
| 0xf7 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "6"; operand2 = Some "A"; size = 2; cycles = "8"; }
| 0xc9 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "1"; operand2 = Some "C"; size = 2; cycles = "8"; }
| 0xf3 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "6"; operand2 = Some "E"; size = 2; cycles = "8"; }
| 0xd0 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "2"; operand2 = Some "B"; size = 2; cycles = "8"; }
| 0x9f -> { mnemo = "RES"; operand_count = 2; operand1 = Some "3"; operand2 = Some "A"; size = 2; cycles = "8"; }
| 0x9e -> { mnemo = "RES"; operand_count = 2; operand1 = Some "3"; operand2 = Some "(HL)"; size = 2; cycles = "16"; }
| 0x9d -> { mnemo = "RES"; operand_count = 2; operand1 = Some "3"; operand2 = Some "L"; size = 2; cycles = "8"; }
| 0x9c -> { mnemo = "RES"; operand_count = 2; operand1 = Some "3"; operand2 = Some "H"; size = 2; cycles = "8"; }
| 0x9b -> { mnemo = "RES"; operand_count = 2; operand1 = Some "3"; operand2 = Some "E"; size = 2; cycles = "8"; }
| 0x9a -> { mnemo = "RES"; operand_count = 2; operand1 = Some "3"; operand2 = Some "D"; size = 2; cycles = "8"; }
| 0xd7 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "2"; operand2 = Some "A"; size = 2; cycles = "8"; }
| 0xd8 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "3"; operand2 = Some "B"; size = 2; cycles = "8"; }
| 0xd9 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "3"; operand2 = Some "C"; size = 2; cycles = "8"; }
| 0xdb -> { mnemo = "SET"; operand_count = 2; operand1 = Some "3"; operand2 = Some "E"; size = 2; cycles = "8"; }
| 0x84 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "0"; operand2 = Some "H"; size = 2; cycles = "8"; }
| 0x85 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "0"; operand2 = Some "L"; size = 2; cycles = "8"; }
| 0x86 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "0"; operand2 = Some "(HL)"; size = 2; cycles = "16"; }
| 0x87 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "0"; operand2 = Some "A"; size = 2; cycles = "8"; }
| 0x80 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "0"; operand2 = Some "B"; size = 2; cycles = "8"; }
| 0x81 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "0"; operand2 = Some "C"; size = 2; cycles = "8"; }
| 0x82 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "0"; operand2 = Some "D"; size = 2; cycles = "8"; }
| 0x83 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "0"; operand2 = Some "E"; size = 2; cycles = "8"; }
| 0x1f -> { mnemo = "RR"; operand_count = 1; operand1 = Some "A"; operand2 = None; size = 2; cycles = "8"; }
| 0x1e -> { mnemo = "RR"; operand_count = 1; operand1 = Some "(HL)"; operand2 = None; size = 2; cycles = "16"; }
| 0x1d -> { mnemo = "RR"; operand_count = 1; operand1 = Some "L"; operand2 = None; size = 2; cycles = "8"; }
| 0x1c -> { mnemo = "RR"; operand_count = 1; operand1 = Some "H"; operand2 = None; size = 2; cycles = "8"; }
| 0x1b -> { mnemo = "RR"; operand_count = 1; operand1 = Some "E"; operand2 = None; size = 2; cycles = "8"; }
| 0x1a -> { mnemo = "RR"; operand_count = 1; operand1 = Some "D"; operand2 = None; size = 2; cycles = "8"; }
| 0xde -> { mnemo = "SET"; operand_count = 2; operand1 = Some "3"; operand2 = Some "(HL)"; size = 2; cycles = "16"; }
| 0xdf -> { mnemo = "SET"; operand_count = 2; operand1 = Some "3"; operand2 = Some "A"; size = 2; cycles = "8"; }
| 0xd1 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "2"; operand2 = Some "C"; size = 2; cycles = "8"; }
| 0xd2 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "2"; operand2 = Some "D"; size = 2; cycles = "8"; }
| 0xd3 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "2"; operand2 = Some "E"; size = 2; cycles = "8"; }
| 0xd4 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "2"; operand2 = Some "H"; size = 2; cycles = "8"; }
| 0xd5 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "2"; operand2 = Some "L"; size = 2; cycles = "8"; }
| 0x8d -> { mnemo = "RES"; operand_count = 2; operand1 = Some "1"; operand2 = Some "L"; size = 2; cycles = "8"; }
| 0x8e -> { mnemo = "RES"; operand_count = 2; operand1 = Some "1"; operand2 = Some "(HL)"; size = 2; cycles = "16"; }
| 0x8f -> { mnemo = "RES"; operand_count = 2; operand1 = Some "1"; operand2 = Some "A"; size = 2; cycles = "8"; }
| 0xe0 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "4"; operand2 = Some "B"; size = 2; cycles = "8"; }
| 0xe7 -> { mnemo = "SET"; operand_count = 2; operand1 = Some "4"; operand2 = Some "A"; size = 2; cycles = "8"; }
| 0x8a -> { mnemo = "RES"; operand_count = 2; operand1 = Some "1"; operand2 = Some "D"; size = 2; cycles = "8"; }
| 0x19 -> { mnemo = "RR"; operand_count = 1; operand1 = Some "C"; operand2 = None; size = 2; cycles = "8"; }
| 0x18 -> { mnemo = "RR"; operand_count = 1; operand1 = Some "B"; operand2 = None; size = 2; cycles = "8"; }
| 0x17 -> { mnemo = "RL"; operand_count = 1; operand1 = Some "A"; operand2 = None; size = 2; cycles = "8"; }
| 0x16 -> { mnemo = "RL"; operand_count = 1; operand1 = Some "(HL)"; operand2 = None; size = 2; cycles = "16"; }
| 0x15 -> { mnemo = "RL"; operand_count = 1; operand1 = Some "L"; operand2 = None; size = 2; cycles = "8"; }
| 0x14 -> { mnemo = "RL"; operand_count = 1; operand1 = Some "H"; operand2 = None; size = 2; cycles = "8"; }
| 0x13 -> { mnemo = "RL"; operand_count = 1; operand1 = Some "E"; operand2 = None; size = 2; cycles = "8"; }
| 0x12 -> { mnemo = "RL"; operand_count = 1; operand1 = Some "D"; operand2 = None; size = 2; cycles = "8"; }
| 0x11 -> { mnemo = "RL"; operand_count = 1; operand1 = Some "C"; operand2 = None; size = 2; cycles = "8"; }
| 0x10 -> { mnemo = "RL"; operand_count = 1; operand1 = Some "B"; operand2 = None; size = 2; cycles = "8"; }
| 0x97 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "2"; operand2 = Some "A"; size = 2; cycles = "8"; }
| 0x96 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "2"; operand2 = Some "(HL)"; size = 2; cycles = "16"; }
| 0x95 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "2"; operand2 = Some "L"; size = 2; cycles = "8"; }
| 0x94 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "2"; operand2 = Some "H"; size = 2; cycles = "8"; }
| 0x93 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "2"; operand2 = Some "E"; size = 2; cycles = "8"; }
| 0x92 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "2"; operand2 = Some "D"; size = 2; cycles = "8"; }
| 0x91 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "2"; operand2 = Some "C"; size = 2; cycles = "8"; }
| 0x90 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "2"; operand2 = Some "B"; size = 2; cycles = "8"; }
| 0x99 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "3"; operand2 = Some "C"; size = 2; cycles = "8"; }
| 0x98 -> { mnemo = "RES"; operand_count = 2; operand1 = Some "3"; operand2 = Some "B"; size = 2; cycles = "8"; }
| _ -> raise Not_found
