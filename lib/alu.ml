open Registers
open Cpu
open State
type opcode_res = State.t * int

let ld_n ({ cpu; _ } as t) reg =
  Mmu.get_n t cpu.pc |> set_register cpu reg;
  { t with cpu = { cpu with pc = succ cpu.pc; }; },
  8

let ld_n_hl ({ cpu; _ } as t) =
  let addr = get_register_pair cpu `Hl in
  Mmu.get_n t cpu.pc |> Mmu.put_n t addr;
  { t with cpu = { cpu with pc = succ cpu.pc; }; },
  4
  
let ld_rr ({ cpu; _ } as t) reg1 reg2 =
  set_register cpu reg1 (get_register cpu reg2);
  t, 4

let ld_nn_sp ({ cpu; _ } as t) =
  let v = cpu.sp in
  let nn = Mmu.get_nn t cpu.pc in
  Mmu.put_nn t nn v;
  { t with cpu = { cpu with pc = cpu.pc + 2; }; },
  20

let ld_sp ({ cpu; _ } as t) =
  let sp = Mmu.get_nn t cpu.pc in
  { t with cpu = { cpu with pc = cpu.pc + 2; sp; }; },
  12

let ld_p ({ cpu; _ } as t) pair reg =
  Mmu.get_n t (get_register_pair cpu pair) |> set_register cpu reg;
  t, 8
  
let ld_0xFF00_r_ra ({ cpu; _ } as t) reg1 reg2 =
  let r1 = get_register cpu reg1 in
  let r2 = get_register cpu reg2 in
  Mmu.put_n t (0xFF00 + Char.code r2) r1;
  t, 8
  
let ld_0xFF00_ra_r ({ cpu; _ } as t) reg1 reg2 =
  let r1 = get_register cpu reg1 in
  set_register cpu reg2 (Mmu.get_n t @@ 0xFF00 + Char.code r1);
  t, 8
  
let ld_0xFF00_r_n ({ cpu; _ } as t) reg =
  let n = Mmu.get_n t cpu.pc in
  Mmu.put_n t (0xFF00 + Char.code n) (get cpu.rg reg);
  {
    t with
    cpu =  { cpu with pc = succ cpu.pc; };
  },
  12

let ld_0xFF00_n_r ({ cpu; _ } as t) reg =
  let n = Mmu.get_n t cpu.pc in
  set_register cpu reg (Mmu.get_n t @@ 0xFF00 + Char.code n);
  {
    t with
    cpu = { cpu with pc = succ cpu.pc; };
  },
  12

let ldi_hl_r ({ cpu; _ } as t) reg =
  let r = get_register cpu reg in
  let hl = get_register_pair cpu `Hl in
  Mmu.put_n t hl r;
  set_register_pair cpu `Hl (succ hl);
  t, 8
  
let xor_n ({ cpu; _ } as t) reg1 =
  let open Uint8 in
  let r1 = get_register cpu reg1 in
  let r2 = Mmu.get_n t cpu.pc in
  let res = r2 lxor r1 in
  write_flags ~z:(res = Uint8.zero) ~n:false ~hc:false ~ca:false cpu.rg;
  set cpu.rg reg1 res;
  {
    t with
    cpu = { cpu with pc = cpu.pc + 1; };
  },
  8

let xor_hl ({ cpu; _ } as t) reg1 =
  let open Uint8 in
  let r1 = get_register cpu reg1 in
  let r2 = Mmu.get_n t (get_register_pair cpu `Hl) in
  let res = r2 lxor r1 in
  write_flags ~z:(res = Uint8.zero) ~n:false ~hc:false ~ca:false cpu.rg;
  set cpu.rg reg1 res;
  t, 8

let xor ({ cpu; _ } as t) reg1 reg2 =
  let open Uint8 in
  let r1 = get_register cpu reg1 in
  let r2 = get_register cpu reg2 in
  let res = r2 lxor r1 in
  write_flags ~z:(res = Uint8.zero) ~n:false ~hc:false ~ca:false cpu.rg;
  set cpu.rg reg1 res;
  t, 4

let ld_nn ({ cpu; _ } as t) pair =
  Mmu.get_nn t cpu.pc |> set_register_pair cpu pair;
  {
    t with
    cpu = { cpu with pc = cpu.pc + 2; };
  },
  12

let ld_hl_sp ({ cpu; _ } as t) =
  let hl = get_register_pair cpu `Hl in
  {
    t with
    cpu = { cpu with sp = hl; };
  },
  8


let ld_hl_sp_n ({ cpu; _ } as t) =
  let is_ca r1 r2 =
    let r1 = r1 land 0xFF in
    let r2 = r2 land 0xFF in
    r1 + r2 > 0xff
  in

  let is_hc r1 r2 =
    let r1 = r1 land 0xFF in
    let r2 = r2 land 0xFF in
    (r1 land 0xf) + (r2 land 0xf) > 0xf
  in
  let r2 = Mmu.get_n t cpu.pc in
  let n =
    if Uint8.code r2 > 0x80 then
      -(0x80 - (Uint8.code r2 - 0x80))
    else
      Uint8.code r2
  in
  let v = cpu.sp + n in
  set_register_pair cpu `Hl v;
  let hc = is_hc cpu.sp n in
  let ca = is_ca cpu.sp n in
  write_flags cpu.rg ~ca ~hc ~z:false ~n:false;
  {
    t with
    cpu = { cpu with pc = cpu.pc + 1; };
  },
  8

let ldd ({ cpu; _ } as t) reg pair =
  let hl = get_register_pair cpu pair in
  get_register cpu reg |> Mmu.put_n t hl;
  set_register_pair cpu `Hl (hl - 1);
  t, 8
  
let ldd_hl ({ cpu; _ } as t) pair reg =
  let hl = get_register_pair cpu pair in
  let v = Mmu.get_n t hl in
  set_register cpu reg v;
  set_register_pair cpu pair (hl - 1);
  t, 8
  
let bit ({ cpu; _ } as t) reg bit =
  let r = get_register cpu reg in
  let z = not (Uint8.is_bit_set r bit) in
  write_flags cpu.rg ~z ~hc:true ~n:false;
  t, 8

let jr ({ cpu; _ } as t) flag cond =
  let cycles, pc =
    let imm = Mmu.get_n t cpu.pc in
    if (is_flag_set cpu.rg flag) = cond then
      12, succ cpu.pc + (Uint8.to_signed imm)
    else
      8, succ cpu.pc
  in
  {
    t with
    cpu = { cpu with pc; };
  },
  cycles

let jrz ({ cpu; _ } as t) flag =
  let cycles, pc =
    let imm = Mmu.get_n t cpu.pc in
    if (is_flag_set cpu.rg flag) then
      12, succ cpu.pc + (Uint8.to_signed imm)
    else
      8, succ cpu.pc
  in
  {
    t with
    cpu = { cpu with pc; };
  },
  cycles

let jr_n ({ cpu; _ } as t) =
  let imm = Mmu.get_n t cpu.pc in
  let pc = succ cpu.pc + (Uint8.to_signed imm) in
  {
    t with
    cpu = { cpu with pc; };
  },
  8

let jp_c ({ cpu; _ } as t) cond =
  let imm = Mmu.get_nn t cpu.pc in
  let check = is_flag_set cpu.rg Registers.ca = cond in
  let pc = if check then imm else cpu.pc + 2 in
  {
    t with
    cpu = { cpu with pc; };
  },
  12

let jp_z ({ cpu; _ } as t) cond =
  let imm = Mmu.get_nn t cpu.pc in
  let check = is_flag_set cpu.rg Registers.z = cond in
  let pc = if check then imm else cpu.pc + 2 in
  {
    t with
    cpu = { cpu with pc; };
  },
  12

let jp_nn ({ cpu; _ } as t) =
  let nn = Mmu.get_nn t cpu.pc in
  let pc = nn in
  {
    t with
    cpu = { cpu with pc; };
  },
  12

let jp_hl ({ cpu; _ } as t) =
  let hl = get_register_pair cpu `Hl in
  let pc = hl in
  {
    t with
    cpu = { cpu with pc; };
  },
  8

let inc ({ cpu; _ } as t) reg =
  let r = get_register cpu reg in
  let res = Uint8.succ r in
  set_register cpu reg res;
  let hc = Uint8.((res land (Uint8.chr 0x0F)) < (r land (Uint8.chr 0x0F))) in
  write_flags ~z:(res = Uint8.zero) ~n:false ~hc cpu.rg;
  t, 4
  
let inc_p ({ cpu; _ } as t) p =
  get_register_pair cpu p |> succ |> set_register_pair cpu p;
  t, 4

let inc_sp ({ cpu; _ } as t) =
  { t with cpu = { cpu with sp = Uint16.add cpu.sp 1; }; },
  4

let dec_sp ({ cpu; _ } as t) =
  { t with cpu = { cpu with sp = Uint16.sub cpu.sp 1; }; },
  4
  
let dec ({ cpu; _ } as t) reg =
  let r = get_register cpu reg in
  let v = Uint8.pred r in
  set_register cpu reg v;
  let hc = Uint8.((v land (Uint8.chr 0x0F)) > (r land (Uint8.chr 0x0F))) in
  write_flags cpu.rg ~z:(v = Uint8.zero) ~n:true ~hc;
  t, 4
  
let dec_hl ({ cpu; _ } as t) =
  let a = get_register_pair cpu `Hl in
  let v = Mmu.get_n t a in
  let v' = Uint8.(pred v) in
  write_flags cpu.rg ~z:(v' = Uint8.zero) ~n:true ~hc:Uint8.(v' land chr 0xf = chr 0xf);
  Mmu.put_n t a v';
  t, 12
  
let inc_hl ({ cpu; _ } as t) =
  let a = get_register_pair cpu `Hl in
  let v = Mmu.get_n t a in
  let v' = Uint8.(succ v) in
  write_flags ~z:(v' = Uint8.zero) ~n:false ~hc:Uint8.(v' land chr 0xf = zero) cpu.rg;
  Mmu.put_n t a v';
  t, 12
  
let dec_p ({ cpu; _ } as t) p =
  let v = get_register_pair cpu p in
  let v' = v - 1 in
  set_register_pair cpu p v';
  t, 8
  
let ld_p_nn ({ cpu; _ } as t) p reg =
  let hl = get_register_pair cpu p in
  get_register cpu reg |> Mmu.put_n t hl;
  t, 8
  
let ld_r_hl ({ cpu; _ } as t) reg =
  let hl = get_register_pair cpu `Hl in
  let v = get_register cpu reg in
  Mmu.put_n t hl v;
  t, 8
  
let ldi_r_hl ({ cpu; _ } as t) reg =
  let hl = get_register_pair cpu `Hl in
  let v = Mmu.get_n t hl in
  set_register_pair cpu `Hl (hl + 1);
  set_register cpu reg v;
  t, 8
  
let ld_r_nn ({ cpu; _ } as t) reg =
  let r = get_register cpu reg in
  let nn = Mmu.get_nn t cpu.pc in
  Mmu.put_n t nn r;
  {
    t with
    cpu = { cpu with pc = cpu.pc + 2; };
  },
  16

let ld_nn_r ({ cpu; _ } as t) reg =
  let nn = Mmu.get_nn t cpu.pc in
  let v = Mmu.get_n t nn in
  set_register cpu reg v;
  {
    t with
    cpu = { cpu with pc = cpu.pc + 2; };
  },
  16

let call_nn ({ cpu; _ } as t) =
  let nn = Mmu.get_nn t cpu.pc in
  Mmu.put_nn t (cpu.sp - 2) (cpu.pc + 2);
  {
    t with
    cpu = { cpu with pc = nn; sp = cpu.sp - 2; };
  },
  12

let call_z ({ cpu; _ } as t) cond =
  if is_flag_set cpu.rg Registers.z = cond then
    call_nn t
  else
    {
      t with
      cpu = { cpu with pc = cpu.pc + 2; };
    },
    12

let call_c ({ cpu; _ } as t) cond =
  if is_flag_set cpu.rg Registers.ca = cond then
    call_nn t
  else
  {
    t with
    cpu = { cpu with pc = cpu.pc + 2; };
  },
  12

let rst ({ cpu; _ } as t) n =
  Mmu.put_nn t (cpu.sp - 2) cpu.pc;
  {
    t with
    cpu = { cpu with pc = n; sp = cpu.sp - 2; };
  },
  12


let push_p ({ cpu; _ } as t) pair =
  let p = get_register_pair cpu pair in
  Mmu.put_nn t (cpu.sp - 2) p;
  {
    t with
    cpu = { cpu with sp = cpu.sp - 2; };
  },
  16

let pop_p ({ cpu; _ } as t) pair =
  set_register_pair cpu pair (Mmu.get_nn t cpu.sp);
  {
    t with
    cpu = { cpu with sp = cpu.sp + 2; };
  },
  12

let ret ({ cpu; _ } as t) =
  let pc = Mmu.get_nn t cpu.sp in
  let sp = cpu.sp + 2 in
  {
    t with 
    cpu = { cpu with pc; sp; };
  },
  16

let reti ({ cpu; _ } as t) =
  let pc = Mmu.get_nn t cpu.sp in
  let sp = cpu.sp + 2 in
  {
    t with
    cpu = { cpu with ime = true; pc; sp; };
  },
  16
  
let ret_z ({ cpu; _ } as t) cond =
  let check = is_flag_set cpu.rg Registers.z = cond in
  if check then
    ret t
  else
    t, 12
    
let ret_c ({ cpu; _ } as t) cond =
  let check = is_flag_set cpu.rg Registers.ca = cond in
  if check then
    ret t
  else
    t, 12
    
let rlca ({ cpu; _ } as t) =
  let open Uint8 in
  let v = get_register cpu a in
  let ca = is_bit_set v 7 in
  let v' = v lsl one in
  let v' = if ca then set_bit v' 0 else v' in
  write_flags cpu.rg  ~ca ~z:false ~n:false ~hc:false;
  set_register cpu a v';
  t,
  4
  
let rrca ({ cpu; _ } as t) =
  let open Uint8 in
  let v = get_register cpu a in
  let c = v land one in
  let v' = (c lsl chr 7) lor (v lsr one) in
  let ca = if c = one then true else false in
  write_flags cpu.rg  ~ca ~z:false ~n:false ~hc:false;
  set_register cpu a v';
  t,
  4
  
let rlc_r ({ cpu; _ } as t) r =
  let open Uint8 in
  let v = get_register cpu r in
  let ca = is_bit_set v 7 in
  let v' = v lsl one in
  let v' = if ca then set_bit v' 0 else v' in
  let z = v' = zero in
  write_flags cpu.rg ~z ~ca ~n:false ~hc:false;
  set_register cpu r v';
  t,
  4
  
let rlc_hl ({ cpu; _ } as t) =
  let open Uint8 in
  let hl = get_register_pair cpu `Hl in
  let v = Mmu.get_n t hl in
  let ca = is_bit_set v 7 in
  let v' = v lsl one in
  let v' = if ca then set_bit v' 0 else v' in
  let z = v' = zero in
  write_flags cpu.rg ~z ~ca ~n:false ~hc:false;
  Mmu.put_n t hl v';
  t,
  16

let rrc_r ({ cpu; _ } as t) r =
  let open Uint8 in
  let v = get_register cpu r in
  let ca = is_bit_set v 0 in
  let v' = v lsr one in
  let v' = if ca then set_bit v' 7 else v' in
  let z = v' = zero in
  write_flags cpu.rg ~z ~ca ~n:false ~hc:false;
  set_register cpu r v';
  t,
  4
  
let rrc_hl ({ cpu; _ } as t) =
  let open Uint8 in
  let hl = get_register_pair cpu `Hl in
  let v = Mmu.get_n t hl in
  let ca = is_bit_set v 0 in
  let v' = v lsr one in
  let v' = if ca then set_bit v' 7 else v' in
  let z = v' = zero in
  write_flags cpu.rg ~z ~ca ~n:false ~hc:false;
  Mmu.put_n t hl v';
  t,
  16
  
let rl_hl ({ cpu; _ } as t) =
  let open Uint8 in
  let hl = get_register_pair cpu `Hl in
  let r = Mmu.get_n t hl in
  let old_carry = is_flag_set cpu.rg ca in
  let r' = if old_carry then set_bit (r lsl one) 0 else r lsl one in
  Mmu.put_n t hl r';
  write_flags cpu.rg ~z:(r' = zero) ~n:false ~hc:false ~ca:(is_bit_set r 7);
  t, 16
  
let rl_r ({ cpu; _ } as t) reg =
  let open Uint8 in
  let r = get_register cpu reg in
  let old_carry = is_flag_set cpu.rg ca in
  let r' = if old_carry then set_bit (r lsl one) 0 else r lsl one in
  set_register cpu reg r';
  write_flags cpu.rg ~z:(r' = zero) ~n:false ~hc:false ~ca:(is_bit_set r 7);
  t, 8
  
let sra_hl ({ cpu; _ } as t) =
  let open Uint8 in
  let hl = get_register_pair cpu `Hl in
  let r = Mmu.get_n t hl in
  let ca = if (r land one) = one then true else false in
  let v' = (to_signed r |> chr) lsr one in
  let z = v' = zero in
  Mmu.put_n t hl v';
  write_flags cpu.rg ~z ~n:false ~hc:false ~ca;
  t, 8
  
let sra_r ({ cpu; _ } as t) reg =
  let open Uint8 in
  let r = get_register cpu reg in
  let ca = if (r land one) = one then true else false in
  let v' = (to_signed r |> chr) lsr one in
  let z = v' = zero in
  set_register cpu reg v';
  write_flags cpu.rg ~z ~n:false ~hc:false ~ca;
  t, 8

let rla ({ cpu; _ } as t) =
  let open Uint8 in
  let r = get_register cpu a in
  let old_carry = if is_flag_set cpu.rg ca then one else zero in
  let ca = (r lsr chr 7) land one in
  let v' = (r lsl one) lor old_carry in
  let ca = if ca = one then true else false in
  set_register cpu a v';
  write_flags cpu.rg ~z:false ~n:false ~hc:false ~ca;
  t, 8
  
let cp_n ({ cpu; _ } as t) =
  let open Uint8 in
  let a = get_register cpu a in
  let n = Mmu.get_n t cpu.pc in
  let hc = (a land (Char.chr 0x0F)) < (n land (Char.chr 0x0F)) in
  write_flags cpu.rg ~z:(n = a) ~n:true ~hc ~ca:(a < n);
  {
    t with 
    cpu = { cpu with pc = cpu.pc + 1; };
  },
  8

let cp ({ cpu; _ } as t) r =
  let open Uint8 in
  let a = get_register cpu a in
  let n = get_register cpu r in
  let hc = (a land (Char.chr 0x0F)) < (n land (Char.chr 0x0F)) in
  write_flags cpu.rg ~z:(n = a) ~n:true ~hc ~ca:(a < n);
  t, 4

let cp_hl ({ cpu; _ } as t) =
  let open Uint8 in
  let a = get_register cpu a in
  let n = Mmu.get_n t (get_register_pair cpu `Hl) in
  let hc = (a land (Char.chr 0x0F)) < (n land (Char.chr 0x0F)) in
  write_flags cpu.rg ~z:(n = a) ~n:true ~hc ~ca:(a < n);
  t, 8
  
let sub_r ({ cpu; _ } as t) reg =
  let open Uint8 in
  let a = get_register cpu a in
  let r = get_register cpu reg in
  let res = sub a r in
  set_register cpu Registers.a (res);
  let hc = (res land (Char.chr 0x0F)) > (a land (Char.chr 0x0F)) in
  write_flags cpu.rg ~z:(a = r) ~hc ~n:true ~ca:(res > a);
  t, 8
  
let sub_hl ({ cpu; _ } as t) =
  let open Uint8 in
  let a = get_register cpu a in
  let r = Mmu.get_n t (get_register_pair cpu `Hl) in
  let res = sub a r in
  set_register cpu Registers.a (res);
  let hc = (res land (Char.chr 0x0F)) > (a land (Char.chr 0x0F)) in
  write_flags cpu.rg ~z:(a = r) ~hc ~n:true ~ca:(res > a);
  t, 8
  
let add_hl ({ cpu; _ } as t) =
  let open Uint8 in
  let a = get_register cpu a in
  let v = Mmu.get_n t (get_register_pair cpu `Hl) in
  let res = add a v in
  set_register cpu Registers.a res;
  let hc = (res land (Char.chr 0x0F)) < (a land (Char.chr 0x0F)) in
  let ca = res < a in
  write_flags cpu.rg ~n:false ~z:(res = zero) ~hc ~ca;
  t, 8
  
let add ({ cpu; _ } as t) reg1 reg2 =
  let open Uint8 in
  let r1 = get_register cpu reg1 in
  let r2 = get_register cpu reg2 in
  let res = add r1 r2 in
  set_register cpu reg2 res;
  let hc = (res land (Char.chr 0x0F)) < (r2 land (Char.chr 0x0F)) in
  let ca = res < r2 in
  write_flags cpu.rg ~n:false ~z:(res = zero) ~hc ~ca;
  t, 4

let add_n ({ cpu; _ } as t) =
  let open Uint8 in
  let r1 = get_register cpu Registers.a in
  let r2 = Mmu.get_n t cpu.pc in
  let res = add r1 r2 in
  set_register cpu Registers.a res;
  let hc = (res land (Char.chr 0x0F)) < (r2 land (Char.chr 0x0F)) in
  let ca = res < r2 in
  write_flags cpu.rg ~n:false ~z:(res = zero) ~hc ~ca;
  {
    t with
    cpu = { cpu with pc = cpu.pc + 1; };
  }, 4

let is_ca r1 r2 =
  let r1 = r1 land 0xFF in
  let r2 = r2 land 0xFF in
  r1 + r2 > 0xff

let is_hc r1 r2 =
  let r1 = r1 land 0xFF in
  let r2 = r2 land 0xFF in
  (r1 land 0xf) + (r2 land 0xf) > 0xf
  
let add_n_sp ({ cpu; _ } as t) =
  let r1 = cpu.sp in
  let r2 = Mmu.get_n t cpu.pc in
  let r2 =
    if Uint8.code r2 > 0x80 then
      -(0x80 - (Uint8.code r2 - 0x80))
    else
      Uint8.code r2
  in
  let sp = Uint16.add r1 r2 in
  let hc = is_hc r1 r2 in
  let ca = is_ca r1 r2 in
  write_flags cpu.rg ~hc ~ca ~n:false ~z:false;
  {
    t with
    cpu = { cpu with pc = cpu.pc + 1; sp; };
  }, 4
  
let sub_n ({ cpu; _ } as t) =
  let open Uint8 in
  let a = get_register cpu a in
  let r = Mmu.get_n t cpu.pc in
  let res = sub a r in
  set_register cpu Registers.a (res);
  let hc = (res land (Char.chr 0x0F)) > (a land (Char.chr 0x0F)) in
  write_flags cpu.rg ~z:(a = r) ~hc ~n:true ~ca:(res > a);
  {
    t with
    cpu = { cpu with pc = cpu.pc + 1; };
  },
  8
  
let res ({ cpu; _ } as t) bit reg =
  get_register cpu reg |> Uint8.unset_bit' bit |> set_register cpu reg;
  t, 8
  
let res_hl ({ cpu; _ } as t) bit =
  let hl = get_register_pair cpu `Hl in
  let v = Mmu.get_n t hl in
  Mmu.put_n t hl (Uint8.unset_bit' bit v);
  t, 8

let noop t = t, 4
                             
let di ({ cpu; _ } as t) =
  {
    t with
    cpu = { cpu with ime = false; };
  }, 4

let ei ({ cpu; _ } as t) =
  let ise = Mmu.get_n t 0xFFFF in
  Mmu.put_n t 0xFF0F ise;
  {
    t with
    cpu = { cpu with ime = true; };
  }, 4

let or' ({ cpu; _ } as t) reg1 reg2 =
  let open Uint8 in
  let r1 = get_register cpu reg1 in
  let r2 = get_register cpu reg2 in
  let v = r1 lor r2 in
  set_register cpu reg2 v;
  write_flags ~z:(v = zero) ~n:false ~hc:false ~ca:false cpu.rg;
  t, 4
  
let or_n ({ cpu; _ } as t) r =
  let open Uint8 in
  let r1 = get_register cpu r in
  let n = Mmu.get_n t cpu.pc in
  let v = n lor r1 in
  set_register cpu r v;
  write_flags ~z:(v = zero) ~n:false ~hc:false ~ca:false cpu.rg;
  {
    t with
    cpu = { cpu with pc = cpu.pc + 1;};
  },
  4

let or_hl ({ cpu; _ } as t) =
  let open Uint8 in
  let addr = get_register_pair cpu `Hl in
  let r1 = get_register cpu a in
  let n = Mmu.get_n t addr in
  let v = n lor r1 in
  set_register cpu a v;
  write_flags ~z:(v = zero) ~n:false ~hc:false ~ca:false cpu.rg;
  t, 8
  
let and' ({ cpu; _ } as t) reg1 reg2 =
  let open Uint8 in
  let r1 = get_register cpu reg1 in
  let r2 = get_register cpu reg2 in
  let v = r1 land r2 in
  set_register cpu reg2 v;
  write_flags ~z:(v = zero) ~n:false ~hc:true ~ca:false cpu.rg;
  t, 4
  
let and_n ({ cpu; _ } as t)  =
  let open Uint8 in
  let a = get_register cpu Registers.a in
  let n = Mmu.get_n t cpu.pc in
  let v = n land a in
  set_register cpu Registers.a v;
  write_flags ~z:(v = zero) ~n:false ~hc:true ~ca:false cpu.rg;
  {
    t with
    cpu = { cpu with pc = cpu.pc + 1;};
  },
  4

let and_hl ({ cpu; _ } as t)  =
  let open Uint8 in
  let a = get_register cpu Registers.a in
  let n = Mmu.get_n t (get_register_pair cpu `Hl) in
  let v = n land a in
  set_register cpu Registers.a v;
  write_flags ~z:(v = zero) ~n:false ~hc:true ~ca:false cpu.rg;
  t, 4
  
let cpl ({ cpu; _ } as t) =
  let open Uint8 in
  let v = get_register cpu Registers.a in
  let v' = lnot v in
  set_register cpu Registers.a v';
  write_flags ~hc:true ~n:true cpu.rg;
  t, 4

let scf ({ cpu; _ } as t) =
  write_flags ~n:false ~hc:false ~ca:true cpu.rg;
  t, 4
  
let swap_hl ({ cpu; _ } as t) =
  let hl = get_register_pair cpu `Hl in
  let v = Mmu.get_n t hl in
  let v' = Uint8.swap_nibbles v in
  Mmu.put_n t hl v';
  write_flags ~z:(v' = Uint8.zero) ~n:false ~hc:false ~ca:false cpu.rg;
  t, 4
let swap ({ cpu; _ } as t) r =
  let v = get_register cpu r in
  let v' = Uint8.swap_nibbles v in
  set_register cpu r v';
  write_flags ~z:(v' = Uint8.zero) ~n:false ~hc:false ~ca:false cpu.rg;
  t, 4

(* let is_hc a b = Uint8.Infix.(((a land chr 0b1111) + (b land chr 0b1111)) land chr 0b10000) = chr 0b10000;; *)
                
let add_n_hl ({ cpu; _ } as t) p =
  let hl = get_register_pair cpu `Hl  in
  let p = get_register_pair cpu p in
  let res = hl + p in
  let hc = (hl land 0xfff) + (p land 0xfff) > 0xfff in
  write_flags ~n:false ~ca:(p + hl > 0xffff) ~hc cpu.rg;
  set_register_pair cpu `Hl (res land Uint16.max_int);
  t, 8
  
let add_sp_hl ({ cpu; _ } as t) =
  let hl = get_register_pair cpu `Hl  in
  let p = cpu.sp in
  let res = Uint16.add hl p in
  let hc = (hl land 0xfff) + (p land 0xfff) > 0xfff in
  write_flags ~n:false ~ca:(p + hl > 0xffff) ~hc cpu.rg;
  set_register_pair cpu `Hl res;
  t, 8
  
let ld_hl_r ({ cpu; _ } as t) r =
  let hl = Mmu.get_n t (get_register_pair cpu `Hl) in
  set_register cpu r hl;
  t, 4
  
let sla_hl ({ cpu; _ } as t) =
  let open Uint8 in
  let hl = get_register_pair cpu `Hl in
  let v = Mmu.get_n t hl in
  let v' = v lsl one in
  write_flags cpu.rg ~z:(v' = zero) ~hc:false ~n:false ~ca:(is_bit_set v 7);
  Mmu.put_n t hl v';
  t, 8
  
let sla ({ cpu; _ } as t) r =
  let open Uint8 in
  let v = get_register cpu r in
  let v' = v lsl one in
  write_flags cpu.rg ~z:(v' = zero) ~hc:false ~n:false ~ca:(is_bit_set v 7);
  set_register cpu r v';
  t, 8
  
let flags_sbc cpu reg carry n =
  let z = if Uint8.Infix.(reg - n - carry = Uint8.zero) then true else false in
  let hc =
    let open Uint8 in
    code (reg land chr 0xf) - code (n land chr 0xf) - code carry < 0
  in
  let ca = if Uint8.code reg - Uint8.code n - Uint8.code carry < 0 then true else false in
  write_flags cpu.rg ~hc ~z ~n:true ~ca
    
let sbc ({ cpu; _ } as t) r1 r2 =
  let open Uint8 in
  let carry = if is_flag_set cpu.rg ca then one else zero in
  let v = get_register cpu r1 in
  let n = get_register cpu r2 in
  let v' = Infix.(v - n - carry) in
  set_register cpu r1 v';
  flags_sbc cpu v carry n;
  t, 4
  
let sbc_hl ({ cpu; _ } as t) r1 =
  let open Uint8 in
  let carry = if is_flag_set cpu.rg ca then one else zero in
  let v = get_register cpu r1 in
  let n = Mmu.get_n t (get_register_pair cpu `Hl) in
  let v' = Infix.(v - n - carry) in
  set_register cpu r1 v';
  flags_sbc cpu v carry n;
  t, 8
  
let sbc_n ({ cpu; _ } as t) r1 =
  let open Uint8 in
  let carry = if is_flag_set cpu.rg ca then one else zero in
  let v = get_register cpu r1 in
  let n = Mmu.get_n t cpu.pc in
  let v' = Infix.(v - n - carry) in
  set_register cpu r1 v';
  flags_sbc cpu v carry n;
  {
    t with
    cpu = { cpu with pc = cpu.pc + 1; };
  },
  8

let ccf ({ cpu; _ } as t) =
  let ca = is_flag_set cpu.rg ca in
  write_flags ~hc:false ~n:false ~ca:(not ca) cpu.rg;
  t, 4

let daa ({cpu; _ } as t) =
   let a = get_register cpu a in
   let adjust = ref 0 in

   if is_flag_set cpu.rg hc then
     adjust := !adjust lor 0x06;

   if is_flag_set cpu.rg ca then
     adjust := !adjust lor 0x60;

   let res =
     if is_flag_set cpu.rg n then
       Uint8.sub a (Uint8.chr !adjust)

     else begin
	     if Uint8.code a land 0x0F > 0x09 then
        adjust := !adjust lor 0x06;

	    if Uint8.code a > 0x99 then
       adjust := !adjust lor 0x60;

      Uint8.add a (Uint8.chr !adjust)
    end

   in

    let z = res = Uint8.zero in
    let ca = !adjust land 0x60 != 0 in
    let hc = false in
    write_flags  cpu.rg ~z ~hc ~ca;
    set_register cpu Registers.a res;
    t, 4

let set ({ cpu; _ } as t) r i =
  let open Uint8 in
  let v = get_register cpu r in
  set_register cpu r (set_bit v i);
  t, 8
  
let set_hl ({ cpu; _ } as t) i =
  let open Uint8 in
  let a = get_register_pair cpu `Hl in
  let v = Mmu.get_n t a in
  Mmu.put_n t a (set_bit v i);
  t, 12
  
let bit_hl ({ cpu; _ } as t) i =
  let a = get_register_pair cpu `Hl in
  let v = Mmu.get_n t a in
  let z = not (Uint8.is_bit_set v i) in
  write_flags cpu.rg ~z ~n:false ~hc:true;
  t, 8
  
let srl ({ cpu; _ } as t) r =
  let open Uint8 in
  let v = get_register cpu r in
  let v' = v lsr one in
  set_register cpu r v';
  write_flags cpu.rg ~z:(v' = zero) ~n:false ~hc:false ~ca:(is_bit_set v 0);
  t, 4
  
let srl_hl ({ cpu; _ } as t) =
  let open Uint8 in
  let a = get_register_pair cpu `Hl in
  let v = Mmu.get_n t a in
  let v' = v lsr one in
  Mmu.put_n t a v';
  write_flags cpu.rg ~z:(v' = zero) ~n:false ~hc:false ~ca:(is_bit_set v 0);
  t, 8
  
let rr ({ cpu; _ } as t) re =
  let open Uint8 in
  let r = get_register cpu re in
  let old_carry = if is_flag_set cpu.rg ca then one else zero in
  let ca = if (r land one) = one then true else false in
  let v' = (old_carry lsl chr 7) lor (r lsr one) in
  set_register cpu re v';
  write_flags cpu.rg ~n:false ~z:(v' = zero) ~hc:false ~ca;
  t, 8
  
let rra ({ cpu; _ } as t) =
  let open Uint8 in
  let r = get_register cpu a in
  let old_carry = if is_flag_set cpu.rg ca then one else zero in
  let ca = if (r land one) = one then true else false in
  let v' = (old_carry lsl chr 7) lor (r lsr one) in
  set_register cpu a v';
  write_flags cpu.rg ~n:false ~z:false ~hc:false ~ca;
  t, 8
  
let rr_hl ({ cpu; _ } as t) =
  let open Uint8 in
  let a = get_register_pair cpu `Hl in
  let v = Mmu.get_n t a in
  let old_carry = if is_flag_set cpu.rg ca then one else zero in
  let ca = if (v land one) = one then true else false in
  let v' = (old_carry lsl chr 7) lor (v lsr one) in
  Mmu.put_n t a v';
  write_flags cpu.rg ~z:(v' = zero) ~n:false ~hc:false ~ca;
  t, 8

let flags_adc cpu reg carry n =
  let z = if Uint8.Infix.(carry + reg + n = Uint8.zero) then true else false in
  let hc =
    let open Uint8 in
    let open Infix in
    (n land chr 0xf) + (reg land chr 0xf) + carry > chr 0xf
  in
  let ca = if Uint8.code carry + Uint8.code reg + Uint8.code n > 0xff then true else false in
  write_flags cpu.rg ~hc ~z ~n:false ~ca
    
let adc ({ cpu; _ } as t) r1 r2 =
  let open Uint8 in
  let v = get_register cpu r1 in
  let n = get_register cpu r2 in
  let carry = if is_flag_set cpu.rg ca then one else zero in
  let v' = Infix.(v + n + carry) in
  set_register cpu r1 v';
  flags_adc cpu v carry n;
  t, 4
  
let adc_hl ({ cpu; _ } as t) r =
  let open Uint8 in
  let v = get_register cpu r in
  let n = Mmu.get_n t (get_register_pair cpu `Hl) in
  let carry = if is_flag_set cpu.rg ca then one else zero in
  let v' = Infix.(v + n + carry) in
  set_register cpu r v';
  flags_adc cpu v carry n;
  t, 8
  
let adc_n ({ cpu; _ } as t) r =
  let open Uint8 in
  let v = get_register cpu r in
  let n = Mmu.get_n t cpu.pc in
  let carry = if is_flag_set cpu.rg ca then one else zero in
  let v' = Infix.(v + n + carry) in
  set_register cpu r v';
  flags_adc cpu v carry n;
  {
    t with
    cpu = { cpu with pc = cpu.pc + 1; };
  },
  8

let halt ({cpu; _ } as t) =
  { t with
    cpu = { cpu with halted = true; };
  },
  4
  
let dispatch (s : State.t) instr =
  match instr with
  | Instructions.Ld_n reg         -> ld_n s reg
  | Ldi_r_hl reg                  -> ldi_r_hl s reg
  | Ld_rr (r1, r2)                -> ld_rr s r1 r2
  | Ld_sp                         -> ld_sp s
  | Ld_p (pair, reg)              -> ld_p s pair reg
  | Ld_0xFF00_r_ra (reg1, reg2)   -> ld_0xFF00_r_ra s reg1 reg2
  | Ld_0xFF00_ra_r (reg1, reg2)   -> ld_0xFF00_ra_r s reg1 reg2
  | Ld_0xFF00_r_n reg             -> ld_0xFF00_r_n s reg
  | Ld_0xFF00_n_r reg             -> ld_0xFF00_n_r s reg
  | Ldi_hl_r reg                  -> ldi_hl_r s reg
  | Xor (reg1, reg2)              -> xor s reg1 reg2
  | Xor_n r                       -> xor_n s r
  | Xor_hl r                      -> xor_hl s r
  | Srl r                         -> srl s r
  | Srl_hl                        -> srl_hl s
  | Rra                           -> rra s
  | Rr r                          -> rr s r
  | Rr_hl                         -> rr_hl s
  | Ld_nn pair                    -> ld_nn s pair
  | Ldd (reg, pair)               -> ldd s reg pair
  | Ldd_hl (pair, reg)            -> ldd_hl s pair reg
  | Jr (flag, n)                  -> jr s flag n
  | Jrz (flag   )                 -> jrz s flag
  | Jr_n                          -> jr_n s
  | Jp_nn                         -> jp_nn s
  | Jp_z flag                     -> jp_z s flag
  | Jp_c flag                     -> jp_c s flag
  | Ld_hl_sp_n                    -> ld_hl_sp_n s
  | Ld_hl_sp                      -> ld_hl_sp s
  | Jp_hl                         -> jp_hl s
  | Inc reg                       -> inc s reg
  | Inc_sp                        -> inc_sp s
  | Dec_sp                        -> dec_sp s
  | Inc_p p                       -> inc_p s p
  | Dec reg                       -> dec s reg
  | Dec_p p                       -> dec_p s p
  | Or (r1, r2)                   -> or' s r1 r2
  | Or_n r1                       -> or_n s r1
  | Or_hl                         -> or_hl s
  | Ld_p_nn (p, reg)              -> ld_p_nn s p reg
  | Ld_nn_r reg                   -> ld_nn_r s reg
  | Ld_r_nn reg                   -> ld_r_nn s reg
  | Call_nn                       -> call_nn s
  | Call_z cond                   -> call_z s cond
  | Call_c cond                   -> call_c s cond
  | Push_p p                      -> push_p s p
  | Pop_p p                       -> pop_p s p
  | Ret                           -> ret s
  | Reti                          -> reti s
  | Ret_c cond                    -> ret_c s cond
  | Ret_z cond                    -> ret_z s cond
  | Rl_r reg                      -> rl_r s reg
  | Rl_hl                         -> rl_hl s
  | Rla                           -> rla s
  | Cp_n                          -> cp_n s
  | Cp_hl                         -> cp_hl s
  | Cp r                          -> cp s r
  | Sub_r reg                     -> sub_r s reg
  | Add_hl                        -> add_hl s
  | Res (bit, reg)                -> res s bit reg
  | Noop                          -> noop s
  | Ld_n_hl                       -> ld_n_hl s
  | Add (reg1, reg2)              -> add s reg1 reg2
  | Add_n                         -> add_n s
  | Sub_n                         -> sub_n s
  | Sub_hl                        -> sub_hl s
  | Di                            -> di s
  | Ei                            -> ei s
  | Adc (reg1, reg2)              -> adc s reg1 reg2
  | Set (r, i)                    -> set s r i
  | Set_hl i                      -> set_hl s i
  | Adc_n r                       -> adc_n s r
  | Adc_hl r                      -> adc_hl s r
  | Sbc_hl reg1                   -> sbc_hl s reg1
  | Sbc (reg1, reg2)              -> sbc s reg1 reg2
  | Sbc_n reg1                    -> sbc_n s reg1
  | Cpl                           -> cpl s
  | And (reg1, reg2)              -> and' s reg1 reg2
  | And_n                         -> and_n s
  | And_hl                        -> and_hl s
  | Add_n_hl p                    -> add_n_hl s p
  | Add_sp_hl                     -> add_sp_hl s
  | Scf                           -> scf s
  | Swap r                        -> swap s r
  | Rst n                         -> rst s n
  | Ld_hl_r r                     -> ld_hl_r s r
  | Dec_hl                        -> dec_hl s
  | Inc_hl                        -> inc_hl s
  | Ld_nn_sp                      -> ld_nn_sp s
  | Sla r                         -> sla s r
  | Bit (i, r)                    -> bit s r i
  | Bit_hl i                      -> bit_hl s i
  | Ccf                           -> ccf s
  | Daa                           -> daa s
  | Rlc                           -> rlca s
  | Rlc_hl                        -> rlc_hl s
  | Rrca                          -> rrca s
  | Rrc_r r                       -> rrc_r s r
  | Rrc_hl                        -> rrc_hl s
  | Rlc_n r                       -> rlc_r s r
  | Ld_r_hl r                     -> ld_r_hl s r
  | Add_n_sp                      -> add_n_sp s
  | Sra_r r                       -> sra_r s r
  | Sra_hl                        -> sra_hl s
  | Sla_hl                        -> sla_hl s
  | Swap_hl                       -> swap_hl s
  | Res_hl i                      -> res_hl s i
  | Halt                          -> halt s
  | Unk c ->
    Printf.printf "dispatch: Unknown instruction %s\n" (Char.chr c |> Uint8.show_hex);
    raise Not_found
  | instr ->
    Printf.printf "dispatch: not impl: %s\n" (Instructions.sexp_of_instruction instr |> Sexplib.Sexp.to_string);
    assert false
