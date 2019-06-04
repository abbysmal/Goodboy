open Registers
open Sexplib.Std

exception Unknown_instruction of string

type instruction =
    Ld_n of register
  | Jp_c of bool
  | Jp_z of bool
  | Ld_n_hl
  | Ld_nn_r of register
  | Or of (register * register)
  | Or_n of register
  | Or_hl
  | And of (register * register)
  | And_n
  | And_hl
  | Ld_rr of (register * register)
  | Ld_nn_sp
  | Ld_hl_sp_n
  | Ld_hl_sp
  | Ld_sp
  | Ld_r_hl of register
  | Ld_hl_r of register
  | Ld_r_nn of register
  | Ld_nn of paired_register
  | Ld_p_nn of (paired_register * register)
  | Ldd of (register * paired_register)
  | Ldd_hl of (paired_register * register)
  | Ld_0xFF00_a
  | Ld_0xFF00_r_n of register
  | Ld_0xFF00_n_r of register
  | Ld_0xFF00_ra_r of (register * register)
  | Ld_0xFF00_r_ra of (register * register)
  | Ld_p of (paired_register * register)
  | Ldi_hl_r of register
  | Ldi_r_hl of register
  | Call_nn
  | Push_p of paired_register
  | Pop_p of paired_register
  | Bit of (int * register)
  | Jr of (flag * bool)
  | Jrz of (flag)
  | Jr_n
  | Jp_hl
  | Ret
  | Reti
  | Bit_hl of int
  | Call_c of bool
  | Call_z of bool
  | Ret_z of bool
  | Ret_c of bool
  | Rst of int
  | Noop
  | Rr of register
  | Rra
  | Rr_hl
  | Ccf
  | Cp of register
  | Sra_r of register
  | Sra_hl
  | Rrca
  | Rrc_r of register
  | Rrc_hl
  | Cpl
  | Cp_n
  | Cp_p
  | Cp_hl
  | Srl of register
  | Srl_hl
  | Set of (register * int)
  | Set_hl of int
  | Sbc_hl of register
  | Sbc of (register * register)
  | Sbc_n of register
  | Xor of (register * register)
  | Xor_n of register
  | Xor_hl of register
  | Swap of register
  | Swap_hl
  | Add of (register * register)
  | Add_hl
  | Add_n
  | Sub_n
  | Sub_hl
  | Add_n_hl of paired_register
  | Add_n_sp
  | Add_sp_hl
  | Adc_n of register
  | Adc of (register * register)
  | Adc_hl of register
  | Jp_nn
  | Rla
  | Rlc
  | Rlc_hl
  | Rlc_n of register
  | Rl_r of register
  | Rl_hl
  | Halt
  | Dec of register
  | Dec_p of paired_register
  | Inc_p of paired_register
  | Inc of register
  | Inc_sp
  | Dec_sp
  | Sla of register
  | Sla_hl
  | Daa
  | Res of (int * register)
  | Res_hl of int
  | Sub_r of register
  | Di
  | Ei
  | Scf
  | Dec_hl
  | Inc_hl
  | Unk of int [@@deriving sexp]

(* let instruction_size i =
 *   match i with
 *   | Ld_n _
 *   | Ld_n_hl
 *   | Cp_n
 *   | Add_n
 *   | Sub_n
 *   | Jr_n
 *   | Ld_0xFF00_r_n _
 *   | Jr _
 *   | Jrz _
 *   | And_n
 *   | Ld_hl_sp_n
 *   | Add_n_hl _
 *   | Ld_0xFF00_n_r _ -> 2
 *   | Call_nn
 *   | Call_c _
 *   | Call_z _
 *   | Ld_r_nn _
 *   | Ld_sp
 *   | Ld_nn_sp
 *   | Jp_nn
 *   | Jp_c _
 *   | Jp_z _
 *   | Ld_p_nn _
 *   | Ld_nn_r _
 *   | Ld_nn _ -> 3
 *   | Ld_rr _
 *   | And _
 *   | Jp_hl
 *   | And_hl
 *   | Ld_r_hl _
 *   | Ldd _
 *   | Ld_0xFF00_a
 *   | Ld_0xFF00_ra_r _
 *   | Ld_0xFF00_r_ra _
 *   | Swap _
 *   | Ld_p _
 *   | Ldi_hl_r _
 *   | Push_p _
 *   | Pop_p _
 *   | Bit _
 *   | Ret
 *   | Reti
 *   | Ret_z _
 *   | Ret_c _
 *   | Noop
 *   | Xor _
 *   | Sub_r _
 *   | Adc
 *   | Add_hl
 *   | Rlc
 *   | Rla
 *   | Rlc_r _
 *   | Rl_r _
 *   | Halt
 *   | Dec _
 *   | Inc_p _
 *   | Unk _
 *   | Cp_p
 *   | Ldi_r_hl _
 *   | Cp_hl
 *   | Res _
 *   | Di
 *   | Ei
 *   | Dec_p _
 *   | Or _
 *   | Cpl
 *   | Scf
 *   | Rst _
 *   | Ld_hl_r _
 *   | Add _
 *   | Dec_hl
 *   | Inc_hl
 *   | Daa
 *   | Inc _ -> 1 *)

let compile_extended code =
  match Char.code code with
  | 0x38 -> Srl b
  | 0x39 -> Srl c
  | 0x3A -> Srl d
  | 0x3B -> Srl e
  | 0x3C -> Srl h
  | 0x3D -> Srl l
  | 0x3E -> Srl_hl
  | 0x3F -> Srl a
  | 0xC0 -> Set (b, 0)
  | 0xC1 -> Set (c, 0)
  | 0xC2 -> Set (d, 0)
  | 0xC3 -> Set (e, 0)
  | 0xC4 -> Set (h, 0)
  | 0xC5 -> Set (l, 0)
  | 0xC6 -> Set_hl 0
  | 0xC7 -> Set (a, 0)
  | 0xC8 -> Set (b, 1)
  | 0xC9 -> Set (c, 1)
  | 0xCA -> Set (d, 1)
  | 0xCB -> Set (e, 1)
  | 0xCC -> Set (h, 1)
  | 0xCD -> Set (l, 1)
  | 0xCE -> Set_hl 1
  | 0xCF -> Set (a, 1)
  | 0xD0 -> Set (b, 2)
  | 0xD1 -> Set (c, 2)
  | 0xD2 -> Set (d, 2)
  | 0xD3 -> Set (e, 2)
  | 0xD4 -> Set (h, 2)
  | 0xD5 -> Set (l, 2)
  | 0xD6 -> Set_hl 2
  | 0xD7 -> Set (a, 2)
  | 0xD8 -> Set (b, 3)
  | 0xD9 -> Set (c, 3)
  | 0xDA -> Set (d, 3)
  | 0xDB -> Set (e, 3)
  | 0xDC -> Set (h, 3)
  | 0xDD -> Set (l, 3)
  | 0xDE -> Set_hl 3
  | 0xDF -> Set (a, 3)
  | 0xE0 -> Set (b, 4)
  | 0xE1 -> Set (c, 4)
  | 0xE2 -> Set (d, 4)
  | 0xE3 -> Set (e, 4)
  | 0xE4 -> Set (h, 4)
  | 0xE5 -> Set (l, 4)
  | 0xE6 -> Set_hl 4
  | 0xE7 -> Set (a, 4)
  | 0xE8 -> Set (b, 5)
  | 0xE9 -> Set (c, 5)
  | 0xEA -> Set (d, 5)
  | 0xEB -> Set (e, 5)
  | 0xEC -> Set (h, 5)
  | 0xED -> Set (l, 5)
  | 0xEE -> Set_hl 5
  | 0xEF -> Set (a, 5)
  | 0xF0 -> Set (b, 6)
  | 0xF1 -> Set (c, 6)
  | 0xF2 -> Set (d, 6)
  | 0xF3 -> Set (e, 6)
  | 0xF4 -> Set (h, 6)
  | 0xF5 -> Set (l, 6)
  | 0xF6 -> Set_hl 6
  | 0xF7 -> Set (a, 6)
  | 0xF8 -> Set (b, 7)
  | 0xF9 -> Set (c, 7)
  | 0xFA -> Set (d, 7)
  | 0xFB -> Set (e, 7)
  | 0xFC -> Set (h, 7)
  | 0xFD -> Set (l, 7)
  | 0xFE -> Set_hl 7
  | 0xFF -> Set (a, 7)
  | 0x18 -> Rr b
  | 0x19 -> Rr c
  | 0x1A -> Rr d
  | 0x1B -> Rr e
  | 0x1C -> Rr h
  | 0x1D -> Rr l
  | 0x1F -> Rr a
  | 0x1E -> Rr_hl
  | 0x10 -> Rl_r b
  | 0x11 -> Rl_r c
  | 0x12 -> Rl_r d
  | 0x13 -> Rl_r e
  | 0x14 -> Rl_r h
  | 0x15 -> Rl_r l
  | 0x16 -> Rl_hl
  | 0x17 -> Rl_r a
  | 0x80 -> Res (0, b)
  | 0x81 -> Res (0, c)
  | 0x82 -> Res (0, d)
  | 0x83 -> Res (0, e)
  | 0x84 -> Res (0, h)
  | 0x85 -> Res (0, l)
  | 0x86 -> Res_hl 0
  | 0x87 -> Res (0, a)
  | 0x88 -> Res (1, b)
  | 0x89 -> Res (1, c)
  | 0x8A -> Res (1, d)
  | 0x8B -> Res (1, e)
  | 0x8C -> Res (1, h)
  | 0x8D -> Res (1, l)
  | 0x8E -> Res_hl 1
  | 0x8F -> Res (1, a)
  | 0x90 -> Res (2, b)
  | 0x91 -> Res (2, c)
  | 0x92 -> Res (2, d)
  | 0x93 -> Res (2, e)
  | 0x94 -> Res (2, h)
  | 0x95 -> Res (2, l)
  | 0x96 -> Res_hl 2
  | 0x97 -> Res (2, a)
  | 0x98 -> Res (3, b)
  | 0x99 -> Res (3, c)
  | 0x9A -> Res (3, d)
  | 0x9B -> Res (3, e)
  | 0x9C -> Res (3, h)
  | 0x9D -> Res (3, l)
  | 0x9E -> Res_hl 3
  | 0x9F -> Res (3, a)
  | 0xA0 -> Res (4, b)
  | 0xA1 -> Res (4, c)
  | 0xA2 -> Res (4, d)
  | 0xA3 -> Res (4, e)
  | 0xA4 -> Res (4, h)
  | 0xA5 -> Res (4, l)
  | 0xA6 -> Res_hl 4
  | 0xA7 -> Res (4, a)
  | 0xA8 -> Res (5, b)
  | 0xA9 -> Res (5, c)
  | 0xAA -> Res (5, d)
  | 0xAB -> Res (5, e)
  | 0xAC -> Res (5, h)
  | 0xAD -> Res (5, l)
  | 0xAE -> Res_hl 5
  | 0xAF -> Res (5, a)
  | 0xB0 -> Res (6, b)
  | 0xB1 -> Res (6, c)
  | 0xB2 -> Res (6, d)
  | 0xB3 -> Res (6, e)
  | 0xB4 -> Res (6, h)
  | 0xB5 -> Res (6, l)
  | 0xB6 -> Res_hl 6
  | 0xB7 -> Res (6, a)
  | 0xB8 -> Res (7, b)
  | 0xB9 -> Res (7, c)
  | 0xBA -> Res (7, d)
  | 0xBB -> Res (7, e)
  | 0xBC -> Res (7, h)
  | 0xBD -> Res (7, l)
  | 0xBE -> Res_hl 7
  | 0xBF -> Res (7, a)
  | 0x40 -> Bit (0, b)
  | 0x41 -> Bit (0, c)
  | 0x42 -> Bit (0, d)
  | 0x43 -> Bit (0, e)
  | 0x44 -> Bit (0, h)
  | 0x45 -> Bit (0, l)
  | 0x46 -> Bit_hl 0
  | 0x47 -> Bit (0, a)
  | 0x48 -> Bit (1, b)
  | 0x49 -> Bit (1, c)
  | 0x4A -> Bit (1, d)
  | 0x4B -> Bit (1, e)
  | 0x4C -> Bit (1, h)
  | 0x4D -> Bit (1, l)
  | 0x4E -> Bit_hl 1
  | 0x4F -> Bit (1, a)
  | 0x50 -> Bit (2, b)
  | 0x51 -> Bit (2, c)
  | 0x52 -> Bit (2, d)
  | 0x53 -> Bit (2, e)
  | 0x54 -> Bit (2, h)
  | 0x55 -> Bit (2, l)
  | 0x56 -> Bit_hl 2
  | 0x57 -> Bit (2, a)
  | 0x58 -> Bit (3, b)
  | 0x59 -> Bit (3, c)
  | 0x5A -> Bit (3, d)
  | 0x5B -> Bit (3, e)
  | 0x5C -> Bit (3, h)
  | 0x5D -> Bit (3, l)
  | 0x5E -> Bit_hl 3
  | 0x5F -> Bit (3, a)
  | 0x60 -> Bit (4, b)
  | 0x61 -> Bit (4, c)
  | 0x62 -> Bit (4, d)
  | 0x63 -> Bit (4, e)
  | 0x64 -> Bit (4, h)
  | 0x65 -> Bit (4, l)
  | 0x66 -> Bit_hl 4
  | 0x67 -> Bit (4, a)
  | 0x68 -> Bit (5, b)
  | 0x69 -> Bit (5, c)
  | 0x6A -> Bit (5, d)
  | 0x6B -> Bit (5, e)
  | 0x6C -> Bit (5, h)
  | 0x6D -> Bit (5, l)
  | 0x6E -> Bit_hl 5
  | 0x6F -> Bit (5, a)
  | 0x70 -> Bit (6, b)
  | 0x71 -> Bit (6, c)
  | 0x72 -> Bit (6, d)
  | 0x73 -> Bit (6, e)
  | 0x74 -> Bit (6, h)
  | 0x75 -> Bit (6, l)
  | 0x76 -> Bit_hl 6
  | 0x77 -> Bit (6, a)
  | 0x78 -> Bit (7, b)
  | 0x79 -> Bit (7, c)
  | 0x7A -> Bit (7, d)
  | 0x7B -> Bit (7, e)
  | 0x7C -> Bit (7, h)
  | 0x7D -> Bit (7, l)
  | 0x7E -> Bit_hl 7
  | 0x7F -> Bit (7, a)
  | 0x37 -> Swap a
  | 0x36 -> Swap_hl
  | 0x0E -> Rrc_hl
  | 0x0F -> Rrc_r a
  | 0x08 -> Rrc_r b
  | 0x09 -> Rrc_r c
  | 0x0A -> Rrc_r d
  | 0x0B -> Rrc_r e
  | 0x0C -> Rrc_r h
  | 0x0D -> Rrc_r l
  | 0x30 -> Swap b
  | 0x31 -> Swap c
  | 0x32 -> Swap d
  | 0x33 -> Swap e
  | 0x34 -> Swap h
  | 0x35 -> Swap l
  | 0x2E -> Sra_hl
  | 0x2F -> Sra_r a
  | 0x28 -> Sra_r b
  | 0x29 -> Sra_r c
  | 0x2A -> Sra_r d
  | 0x2B -> Sra_r e
  | 0x2C -> Sra_r h
  | 0x2D -> Sra_r l
  | 0x26 -> Sla_hl
  | 0x27 -> Sla a
  | 0x20 -> Sla b
  | 0x21 -> Sla c
  | 0x22 -> Sla d
  | 0x23 -> Sla e
  | 0x24 -> Sla h
  | 0x25 -> Sla l
  | 0x06 -> Rlc_hl
  | 0x07 -> Rlc_n a
  | 0x00 -> Rlc_n b
  | 0x01 -> Rlc_n c
  | 0x02 -> Rlc_n d
  | 0x03 -> Rlc_n e
  | 0x04 -> Rlc_n h
  | 0x05 -> Rlc_n l
  | c -> Unk c

let compile code =
  let open Registers in
  match code with
  | 0x0F -> Rrca
  | 0x33 -> Inc_sp
  | 0x3B -> Dec_sp
  | 0x07 -> Rlc
  | 0x27 -> Daa
  | 0x00 -> Noop
  | 0xF8 -> Ld_hl_sp_n
  | 0x2A -> Ldi_r_hl a
  | 0x06 -> Ld_n b
  | 0x0E -> Ld_n c
  | 0x3F -> Ccf
  | 0x16 -> Ld_n d
  | 0x1E -> Ld_n e
  | 0x26 -> Ld_n h
  | 0x2E -> Ld_n l
  | 0x3E -> Ld_n a
  | 0x98 -> Sbc (a, b)
  | 0x99 -> Sbc (a, c)
  | 0x9A -> Sbc (a, d)
  | 0x9B -> Sbc (a, e)
  | 0x9C -> Sbc (a, h)
  | 0x9D -> Sbc (a, l)
  | 0x9F -> Sbc (a, a)
  | 0xDE -> Sbc_n a
  | 0x1F -> Rra
  | 0x48 -> Ld_rr (c, b)
  | 0x49 -> Ld_rr (c, c)
  | 0x4A -> Ld_rr (c, d)
  | 0x4B -> Ld_rr (c, e)
  | 0x4C -> Ld_rr (c, h)
  | 0x4D -> Ld_rr (c, l)
  | 0x4F -> Ld_rr (c, a)
  | 0x78 -> Ld_rr (a, b)
  | 0x79 -> Ld_rr (a, c)
  | 0x7A -> Ld_rr (a, d)
  | 0x7B -> Ld_rr (a, e)
  | 0x7C -> Ld_rr (a, h)
  | 0x7D -> Ld_rr (a, l)
  | 0x7F -> Ld_rr (a, a)
  | 0x40 -> Ld_rr (b, b)
  | 0x41 -> Ld_rr (b, c)
  | 0x42 -> Ld_rr (b, d)
  | 0x43 -> Ld_rr (b, e)
  | 0x44 -> Ld_rr (b, h)
  | 0x45 -> Ld_rr (b, l)
  | 0x47 -> Ld_rr (b, a)
  | 0x50 -> Ld_rr (d, b)
  | 0x51 -> Ld_rr (d, c)
  | 0x52 -> Ld_rr (d, d)
  | 0x53 -> Ld_rr (d, e)
  | 0x54 -> Ld_rr (d, h)
  | 0x55 -> Ld_rr (d, l)
  | 0x57 -> Ld_rr (d, a)
  | 0x58 -> Ld_rr (e, b)
  | 0x59 -> Ld_rr (e, c)
  | 0x5A -> Ld_rr (e, d)
  | 0x5B -> Ld_rr (e, e)
  | 0x5C -> Ld_rr (e, h)
  | 0x5D -> Ld_rr (e, l)
  | 0x5F -> Ld_rr (e, a)
  | 0x67 -> Ld_rr (h, a)
  | 0x68 -> Ld_rr (l, b)
  | 0x69 -> Ld_rr (l, c)
  | 0x6A -> Ld_rr (l, d)
  | 0x6B -> Ld_rr (l, e)
  | 0x6C -> Ld_rr (l, h)
  | 0x6D -> Ld_rr (l, l)
  | 0x6F -> Ld_rr (l, a)
  | 0x60 -> Ld_rr (h, b)
  | 0x61 -> Ld_rr (h, c)
  | 0x62 -> Ld_rr (h, d)
  | 0x63 -> Ld_rr (h, e)
  | 0x64 -> Ld_rr (h, h)
  | 0x65 -> Ld_rr (h, l)
  | 0x87 -> Add (a, a)
  | 0x80 -> Add (b, a)
  | 0x81 -> Add (c, a)
  | 0x82 -> Add (d, a)
  | 0x83 -> Add (e, a)
  | 0x84 -> Add (h, a)
  | 0x85 -> Add (l, a)
  | 0x86 -> Add_hl
  | 0x97 -> Sub_r a
  | 0x90 -> Sub_r b
  | 0x91 -> Sub_r c
  | 0x92 -> Sub_r d
  | 0x93 -> Sub_r e
  | 0x94 -> Sub_r h
  | 0x95 -> Sub_r l
  | 0x0A -> Ld_p (`Bc, a)
  | 0x1A -> Ld_p (`De, a)
  | 0x02 -> Ld_p_nn (`Bc, a)
  | 0x12 -> Ld_p_nn (`De, a)
  | 0x77 -> Ld_p_nn (`Hl, a)
  | 0xE0 -> Ld_0xFF00_r_n a
  | 0xF0 -> Ld_0xFF00_n_r a
  | 0xF2 -> Ld_0xFF00_ra_r (c, a)
  | 0xE2 -> Ld_0xFF00_r_ra (a, c)
  | 0x22 -> Ldi_hl_r a
  | 0x17 -> Rla
  | 0x20 -> Jr (z, false)
  | 0x28 -> Jrz z
  | 0x38 -> Jr (ca, true)
  | 0x30 -> Jr (ca, false)
  | 0x18 -> Jr_n
  | 0x88 -> Adc (a, b)
  | 0x89 -> Adc (a, c)
  | 0x8A -> Adc (a, d)
  | 0x8B -> Adc (a, e)
  | 0x8C -> Adc (a, h)
  | 0x8D -> Adc (a, l)
  | 0x8F -> Adc (a, a)
  | 0x8E -> Adc_hl a
  | 0xCE -> Adc_n a
  | 0xC5 -> Push_p `Bc
  | 0xD5 -> Push_p `De
  | 0xE5 -> Push_p `Hl
  | 0xF5 -> Push_p `Af
  | 0xC1 -> Pop_p `Bc
  | 0xD1 -> Pop_p `De
  | 0xE1 -> Pop_p `Hl
  | 0xF1 -> Pop_p `Af
  | 0xC9 -> Ret
  | 0xD9 -> Reti
  | 0xC0 -> Ret_z false
  | 0xC8 -> Ret_z true
  | 0xD0 -> Ret_c false
  | 0xD8 -> Ret_c true
  | 0x01 -> Ld_nn `Bc
  | 0x11 -> Ld_nn `De
  | 0x21 -> Ld_nn `Hl
  | 0x32 -> Ldd (a, `Hl)
  | 0x3A -> Ldd_hl (`Hl, a)
  | 0x70 -> Ld_r_hl b
  | 0x71 -> Ld_r_hl c
  | 0x72 -> Ld_r_hl d
  | 0x73 -> Ld_r_hl e
  | 0x74 -> Ld_r_hl h
  | 0x75 -> Ld_r_hl l
  | 0xEA -> Ld_r_nn a
  | 0xFA -> Ld_nn_r a
  | 0x7E -> Ld_hl_r a
  | 0x46 -> Ld_hl_r b
  | 0x4E -> Ld_hl_r c
  | 0x56 -> Ld_hl_r d
  | 0x5E -> Ld_hl_r e
  | 0x66 -> Ld_hl_r h
  | 0x6E -> Ld_hl_r l
  | 0x76 -> Halt
  | 0xC3 -> Jp_nn
  | 0xE9 -> Jp_hl
  | 0xAE -> Xor_hl a
  | 0xEE -> Xor_n a
  | 0xA8 -> Xor (a, b)
  | 0xA9 -> Xor (a, c)
  | 0xAA -> Xor (a, d)
  | 0xAB -> Xor (a, e)
  | 0xAC -> Xor (a, h)
  | 0xAD -> Xor (a, l)
  | 0xAF -> Xor (a, a)
  | 0xCD -> Call_nn
  | 0xC4 -> Call_z false
  | 0xCC -> Call_z true
  | 0xD4 -> Call_c false
  | 0xDC -> Call_c true
  | 0x31 -> Ld_sp
  | 0x3C -> Inc a
  | 0x04 -> Inc b
  | 0x0C -> Inc c
  | 0x14 -> Inc d
  | 0x1C -> Inc e
  | 0x24 -> Inc h
  | 0x2C -> Inc l
  | 0x03 -> Inc_p `Bc
  | 0x13 -> Inc_p `De
  | 0x23 -> Inc_p `Hl
  | 0x05 -> Dec b
  | 0x0D -> Dec c
  | 0x15 -> Dec d
  | 0x1D -> Dec e
  | 0x25 -> Dec h
  | 0x2D -> Dec l
  | 0x3D -> Dec a
  | 0x0B -> Dec_p `Bc
  | 0x1B -> Dec_p `De
  | 0x2B -> Dec_p `Hl
  | 0xFE -> Cp_n
  | 0xBE -> Cp_hl
  | 0xF3 -> Di
  | 0xFB -> Ei
  | 0x36 -> Ld_n_hl
  | 0xB7 -> Or (a, a)
  | 0xB0 -> Or (b, a)
  | 0xB1 -> Or (c, a)
  | 0xB2 -> Or (d, a)
  | 0xB3 -> Or (e, a)
  | 0xB4 -> Or (h, a)
  | 0xB5 -> Or (l, a)
  | 0xB6 -> Or_hl
  | 0xF6 -> Or_n a
  | 0xA7 -> And (a, a)
  | 0xA0 -> And (b, a)
  | 0xA1 -> And (c, a)
  | 0xA2 -> And (d, a)
  | 0xA3 -> And (e, a)
  | 0xA4 -> And (h, a)
  | 0xA5 -> And (l, a)
  | 0xE6 -> And_n
  | 0xA6 -> And_hl
  | 0x2F -> Cpl
  | 0x37 -> Scf
  | 0xC7 -> Rst 0x0000
  | 0xCF -> Rst 0x0008
  | 0xD7 -> Rst 0x0010
  | 0xDF -> Rst 0x0018
  | 0xE7 -> Rst 0x0020
  | 0xEF -> Rst 0x0028
  | 0xF7 -> Rst 0x0030
  | 0xFF -> Rst 0x0038
  | 0x09 -> Add_n_hl `Bc
  | 0x19 -> Add_n_hl `De
  | 0x29 -> Add_n_hl `Hl
  | 0x39 -> Add_sp_hl
  | 0xC6 -> Add_n
  | 0xD6 -> Sub_n
  | 0x96 -> Sub_hl
  | 0xC2 -> Jp_z false
  | 0xCA -> Jp_z true
  | 0xD2 -> Jp_c false
  | 0xDA -> Jp_c true
  | 0x35 -> Dec_hl
  | 0x34 -> Inc_hl
  | 0x08 -> Ld_nn_sp
  | 0xBF -> Cp a
  | 0xB8 -> Cp b
  | 0xB9 -> Cp c
  | 0xBA -> Cp d
  | 0xBB -> Cp e
  | 0xBC -> Cp h
  | 0xBD -> Cp l
  | 0xF9 -> Ld_hl_sp
  | 0xE8 -> Add_n_sp
  | 0x9E -> Sbc_hl a
  | c -> Unk c
