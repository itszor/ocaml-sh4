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
    r0 - r11                    general purpose  (r11 for caml_c_call temp)
    r12                         allocation pointer
    r13                         allocation limit
    r14                         trap pointer
    r15                         stack pointer

    dr0-dr7			Double-precision float registers
    
    macl, mach, fpul, pr, t	Special/control registers
    
    There are also control registers, and system registers.  We might need to
    model some of those.
*)

let int_reg_name = [|
  "r0"; "r1"; "r2"; "r3"; "r4"; "r5"; "r6"; "r7"; "r8"; "r9"; "r10"; "r11"
|]

let float_reg_name = [|
  "dr0";  "dr1";  "dr2";  "dr3";  "dr4";  "dr5";  "dr6"; "dr7"
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

let num_available_registers = [| 12; 8; 4 |]

let first_available_register = [| 0; 100; 200 |]

let register_name r =
  if r < 100 then int_reg_name.(r) else
  if r < 200 then float_reg_name.(r - 100) else
  special_reg_name.(r - 200)

let rotate_registers = true

(* Representation of hard registers by pseudo-registers *)

let hard_int_reg =
  let v = Array.create 12 Reg.dummy in
  for i = 0 to 11 do v.(i) <- Reg.at_location Int (Reg i) done;
  v

let hard_float_reg =
  let v = Array.create 8 Reg.dummy in
  for i = 0 to 7 do v.(i) <- Reg.at_location Float (Reg(100 + i)) done;
  v

let hard_special_reg =
  let v = Array.create 5 Reg.dummy in
  for i = 0 to 4 do v.(i) <- Reg.at_location Int (Reg(200 + i)) done;
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

(* FIXME for SH4 ABI! *)

let loc_arguments arg =
  calling_conventions 0 7 100 107 outgoing arg
let loc_parameters arg =
  let (loc, ofs) = calling_conventions 0 7 100 107 incoming arg in loc
let loc_results res =
  let (loc, ofs) = calling_conventions 0 7 100 107 not_supported res in loc

(* Calling conventions for C are as for Caml, except that float arguments
   are passed in pairs of integer registers. *)

(* FIXME for SH4 ABI! *)

let loc_external_arguments arg =
  let loc = Array.create (Array.length arg) Reg.dummy in
  let reg = ref 0 in
  let ofs = ref 0 in
  for i = 0 to Array.length arg - 1 do
    match arg.(i).typ with
      Int | Addr as ty ->
        if !reg <= 3 then begin
          loc.(i) <- phys_reg !reg;
          incr reg
        end else begin
          loc.(i) <- stack_slot (outgoing !ofs) ty;
          ofs := !ofs + size_int
        end
    | Float ->
        if !reg <= 2 then begin
          loc.(i) <- phys_reg !reg;
          reg := !reg + 2
        end else begin
          loc.(i) <- stack_slot (outgoing !ofs) Float;
          ofs := !ofs + size_float
        end
  done;
  (loc, !ofs)

let loc_external_results res =
  let (loc, ofs) = calling_conventions 0 0 100 100 not_supported res in loc

let loc_exn_bucket = phys_reg 0

(* Registers destroyed by operations *)

(* FIXME for SH4! *)

let destroyed_at_c_call =               (* r4-r9, d8-d15 preserved *)
  Array.of_list(List.map phys_reg [0;1;2;3;10;11;
				   100;101;102;103;104;105;106;107;
				   200;201;202;203])

let destroyed_at_oper = function
    Iop(Icall_ind | Icall_imm _ | Iextcall(_, true)) -> all_phys_regs
  | Iop(Iextcall(_, false)) -> destroyed_at_c_call
  (* r0 used as temp register for some insns *)
  | Iop(Itailcall_ind)
  | Iop(Itailcall_imm _)
  | Iop(Istackoffset _)
  | Iop(Iconst_float _)-> [|phys_reg 0|]
  | Iop(Iintop(Icheckbound)) -> [|phys_reg 11|]  (* r11 used as temp *)
  | Iop(Ialloc(_)) -> [||]    (* FIXME: Whatever is destroyed by alloc *)
  | _ -> [||]

let destroyed_at_raise = all_phys_regs

(* Maximal register pressure (FIXME: I don't understand these) *)

let safe_register_pressure = function
    Iextcall(_, _) -> 4
  | _ -> 12
let max_register_pressure = function
    Iextcall(_, _) -> [| 4; 4; 0 |]
  | _ -> [| 12; 8; 0 |]

(* Layout of the stack *)

let num_stack_slots = [| 0; 0; 0 |]
let contains_calls = ref false

(* Calling the assembler *)

let assemble_file infile outfile =
  Ccomp.command (Config.asm ^ " -o " ^
                 Filename.quote outfile ^ " " ^ Filename.quote infile)

open Clflags;;
open Config;;
