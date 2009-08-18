(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Description of the SH4 processor *)

open Misc
open Cmm
open Reg
open Arch
open Mach

(* Instruction selection *)

let word_addressed = false

(* Registers available for register allocation *)

(* Register map (in progress!)
    r0 - r9                     general purpose
    r10 - r11			temporaries
    r12                         allocation pointer
    r13                         allocation limit
    r14                         trap pointer
    r15                         stack pointer

    dr0, dr2...dr14		Double-precision float registers
    
    macl, mach, fpul, pr, t	Special/control registers
    
   Special registers are only ever used when they are referred to directly.
   
   The C calling conventions on SH4 are:
   
    r0 - r3			Return value, caller save
    r4 - r7			Parameter passing, caller save
    r8 - r13			Callee save
    r12				Global context pointer, callee save
    r14				Frame pointer, callee save
    r15				Stack pointer
    
    fr0 - fr3			Return value, caller save (dr0, dr2)
    fr4 - fr11			Parameter passing, caller save
				(dr4, dr6, dr8, dr10)
    fr12 - fr15			Callee save (dr12, dr14)
*)

let int_reg_name = [|
  "r0"; "r1"; "r2"; "r3"; "r4"; "r5"; "r6"; "r7"; "r8"; "r9"
|]

let float_reg_name = [|
  "dr0";  "dr2";  "dr4";  "dr6";  "dr8";  "dr10";  "dr12"; "dr14"
|]

let special_reg_name = [|
  "macl"; "mach"; "fpul"; "pr"
|]

let num_register_classes = 3

let register_class r =
  match r.typ with
    Int -> 0
  | Addr -> 0
  | Float -> 1

let num_available_registers = [| 10; 8; 0 |]

let first_available_register = [| 0; 100; 200 |]

let register_name r =
  if r < 100 then int_reg_name.(r) else
  if r < 200 then float_reg_name.(r - 100) else
  special_reg_name.(r - 200)

let rotate_registers = true

(* Representation of hard registers by pseudo-registers *)

let hard_int_reg =
  let v = Array.create 10 Reg.dummy in
  for i = 0 to 9 do v.(i) <- Reg.at_location Int (Reg i) done;
  v

let hard_float_reg =
  let v = Array.create 8 Reg.dummy in
  for i = 0 to 7 do v.(i) <- Reg.at_location Float (Reg(100 + i)) done;
  v

let hard_special_reg =
  let v = Array.create 4 Reg.dummy in
  for i = 0 to 3 do v.(i) <- Reg.at_location Int (Reg(200 + i)) done;
  v

let all_phys_regs =
  Array.append (Array.append hard_int_reg hard_float_reg) hard_special_reg

let phys_reg n =
  if n < 100 then hard_int_reg.(n) else
  if n < 200 then hard_float_reg.(n - 100) else
  hard_special_reg.(n - 200)

let stack_slot slot ty =
  Reg.at_location ty (Stack slot)

(* Calling conventions *)

let calling_conventions first_int last_int first_float last_float
                        make_stack arg =
  let loc = Array.create (Array.length arg) Reg.dummy in
  let int = ref first_int in
  let float = ref first_float in
  let ofs = ref 0 in
  for i = 0 to Array.length arg - 1 do
    match arg.(i).typ with
      Int | Addr as ty ->
        if !int <= last_int then begin
          loc.(i) <- phys_reg !int;
          incr int
        end else begin
          loc.(i) <- stack_slot (make_stack !ofs) ty;
          ofs := !ofs + size_int
        end
    | Float ->
        if !float <= last_float then begin
          loc.(i) <- phys_reg !float;
          incr float
        end else begin
          loc.(i) <- stack_slot (make_stack !ofs) Float;
          ofs := !ofs + size_float
        end
  done;
  (loc, !ofs)

let incoming ofs = Incoming ofs
let outgoing ofs = Outgoing ofs
let not_supported ofs = fatal_error "Proc.loc_results: cannot call"

(* Use r0-r7, dr0-dr5 for arguments and results. Chosen somewhat arbitrarily --
   probably worth experimenting with different values. *)

let loc_arguments arg =
  calling_conventions 0 7 100 105 outgoing arg
let loc_parameters arg =
  let (loc, ofs) = calling_conventions 0 7 100 105 incoming arg in loc
let loc_results res =
  let (loc, ofs) = calling_conventions 0 7 100 105 not_supported res in loc

let loc_external_arguments arg =
  let loc = Array.create (Array.length arg) Reg.dummy in
  let ireg = ref 4
  and freg = ref 2 in
  let ofs = ref 0 in
  for i = 0 to Array.length arg - 1 do
    match arg.(i).typ with
      Int | Addr as ty ->
        if !ireg <= 7 then begin
          loc.(i) <- phys_reg !ireg;
          incr ireg
        end else begin
          loc.(i) <- stack_slot (outgoing !ofs) ty;
          ofs := !ofs + size_int
        end
    | Float ->
        (* Float registers fr4-fr11 are used for argument passing.  In our
	   numbering scheme, those are registers 102..105. *)
        if !freg <= 5 then begin
          loc.(i) <- phys_reg (!freg + 100);
          incr freg
        end else begin
          loc.(i) <- stack_slot (outgoing !ofs) Float;
          ofs := !ofs + size_float
        end
  done;
  (loc, !ofs)

(* Return in r0/fr0 for int/float. *)

let loc_external_results res =
  let (loc, ofs) = calling_conventions 0 0 100 100 not_supported res in loc

let loc_exn_bucket = phys_reg 0

(* Registers destroyed by operations *)

(* FIXME for SH4! *)

let destroyed_at_c_call =
  Array.of_list(List.map phys_reg [0;1;2;3;4;5;6;7;
				   100;101;102;103;104;105;
				   200;201;202;203])

let destroyed_at_oper = function
    Iop(Icall_ind | Icall_imm _ | Iextcall(_, true)) -> all_phys_regs
  | Iop(Iextcall(_, false)) -> destroyed_at_c_call
  (* r0 used as temp register for some insns *)
  | Iop(Itailcall_ind)
  | Iop(Itailcall_imm _)
  | Iop(Istackoffset _)
  | Iop(Iconst_float _)-> [|phys_reg 0|]
  | Iop(Ialloc(_)) -> [||]    (* FIXME: Whatever is destroyed by alloc *)
  | _ -> [||]

let destroyed_at_raise = all_phys_regs

(* Maximal register pressure (FIXME: I don't understand these) *)

let safe_register_pressure = function
    Iextcall(_, _) -> 4
  | _ -> 12
let max_register_pressure = function
    Iextcall(_, _) -> [| 4; 4; 0 |]
  | _ -> [| 10; 8; 0 |]

(* Layout of the stack *)

let num_stack_slots = [| 0; 0; 0 |]
let contains_calls = ref false

(* Calling the assembler *)

let assemble_file infile outfile =
  Ccomp.command (Config.asm ^ " -o " ^
                 Filename.quote outfile ^ " " ^ Filename.quote infile)

open Clflags;;
open Config;;
