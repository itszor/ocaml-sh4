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

(* Instruction selection for the SH4 processor *)

open Misc
open Cmm
open Reg
open Arch
open Mach
open Proc

(* Only true for 32-bit accesses:
     mov.l @(disp,Rm),Rn
     mov.l Rm,@(disp,Rn)
*)

let is_offset n =
  (n land 3) = 0 && n >= 0 && n < 64

(* Special constraints on operand and result registers. *)

exception Use_default

let r0 = phys_reg 0
let r1 = phys_reg 1
let macl = phys_reg 200  (* FIXME! *)

(* For operation OP with args ARG and result RES, return:
   (location for args, location for dest, whether to move dest immediately). *)

let pseudoregs_for_operation op arg res =
  match op with
    (* Two-address binary operations *)
    Iintop(Ilsl|Ilsr|Iasr|Iadd|Isub|Iand|Ior|Ixor|Idiv|Imod) ->
      ([|res.(0); arg.(1)|], res, false)
  | Iintop(Imul) ->
      (arg, [|macl|], true)
  | Iintop_imm((Ilsl|Ilsr|Iasr|Iadd), _) ->
      (res, res, false)
  | Iintop_imm((Iand|Ior|Ixor), _) ->
      ([|r0|], [|r0|], true)
  | Iintop_imm(Icomp _, _) ->
      ([|r0|], res, false)
  | Inegf | Iabsf ->
      (res, res, false)
  | Iaddf | Isubf | Imulf | Idivf ->
      ([|res.(0); arg.(1)|], res, false)
  | _ -> raise Use_default

(* Similar, but for tests. Returns location for args. *)

let pseudoregs_for_test op arg =
  match op with
    Itruetest | Ifalsetest | Ioddtest | Ieventest
  | Iinttest_imm ((Isigned Ceq | Isigned Cne
                   | Iunsigned Ceq | Iunsigned Cne), _) ->
      [|r0|]
  | _ -> raise Use_default

(* Instruction selection *)
class selector = object(self)

inherit Selectgen.selector_generic as super

method is_immediate n =
  n >= -128 && n < 128

(* Logic ops don't sign extend their immediate *)

method is_logic_immediate n =
  n >= 0 && n < 256

(* Offset suitable for word loads/stores *)

method is_offset n =
  n land 3 = 0 && n >= 0 && n < 64

method select_addressing = function
    Cop(Cadda, [arg; Cconst_int n]) when is_offset n ->
      (Iindexed n, arg)
  | arg ->
      (Iindirect, arg)

(* Floating-point loads/stores can't use any indexing. *)

method select_float_addressing arg =
  Iindirect, arg

method select_operation op args =
  match op, args with
    Cload (Double | Double_u | Single as chunk), [arg] ->
      let (addr, eloc) = self#select_float_addressing arg in
      (Iload(chunk, addr), [eloc])
  | Cstore (Double | Double_u | Single as chunk), [arg1; arg2] ->
      let (addr, eloc) = self#select_float_addressing arg1 in
      (Istore(chunk, addr), [arg2; eloc])
  | Csuba, _ | Csubi, _ ->
      begin match args with
        [arg1; Cconst_int n] when self#is_immediate (-n) ->
	  (* No immediate sub, but immediate add sign-extends *)
	  (Iintop_imm(Iadd, -n), [arg1])
      | _ -> super#select_operation op args
      end
  | Casr, _ ->
      begin match args with
        [arg1; Cconst_int 1] ->
	  (Iintop_imm(Iasr, 1), [arg1])  (* Can only arith right shift by 1 *)
      | _ -> (Iintop Iasr, args)
      end
  | Ccheckbound _, _ -> (Iintop Icheckbound, args)
  | Cmodi, _ -> (Iextcall("caml_smodsi3", false), args)
  | Cdivi, _ -> (Iextcall("__sdivsi3_i4i", false), args)
  | Cmuli, _ -> (Iintop Imul, args)  (* FIXME: Won't ever use shifts! *)
  | Ccmpi cmp, _ ->
      begin match cmp with
        Cge | Cgt | Cle | Clt -> (Iintop(Icomp(Isigned cmp)), args)
      | _ -> super#select_operation op args
      end
  | Ccmpa cmp, _ ->
      begin match cmp with
        Cge | Cgt | Cle | Clt -> (Iintop(Icomp(Iunsigned cmp)), args)
      | _ -> super#select_operation op args
      end
  | _ -> super#select_operation op args

method select_condition cop =
  match cop with
    Cop((Ccmpi (Cge | Cgt | Cle | Clt as cmp)), ([_; Cconst_int _] as args))
  | Cop((Ccmpi (Cge | Cgt | Cle | Clt as cmp)), ([Cconst_int _; _] as args)) ->
      (Iinttest(Isigned cmp), Ctuple args)
  | Cop((Ccmpa (Cge | Cgt | Cle | Clt as cmp)), ([_; Cconst_int _] as args))
  | Cop((Ccmpa (Cge | Cgt | Cle | Clt as cmp)), ([Cconst_int _; _] as args)) ->
      (Iinttest(Iunsigned cmp), Ctuple args)
  | _ -> super#select_condition cop

(* Bad idea.
method select_store addr arg =
  match addr with
    Iindexed n when not (self#is_offset n) -> (Istore(Word, Iindirect), arg)
  | _ -> super#select_store addr arg
*)

(* Deal with register constraints *)

method insert_debug desc dbg arg res =
  match desc with
    Iifthenelse(econd, eif, eelse) ->
      begin try
        let rsrc = pseudoregs_for_test econd arg in
	self#insert_moves arg rsrc;
	super#insert_debug desc dbg rsrc res;
      with Use_default ->
        super#insert_debug desc dbg arg res
      end
  | _ -> super#insert_debug desc dbg arg res

method insert desc arg res =
  self#insert_debug desc Debuginfo.none arg res

method insert_op_debug op dbg rs rd =
  try
    let (rsrc, rdst, move_res) = pseudoregs_for_operation op rs rd in
    self#insert_moves rs rsrc;
    self#insert_debug (Iop op) dbg rsrc rdst;
    if move_res then begin
      self#insert_moves rdst rd;
      rd
    end else
      rdst
  with Use_default ->
    super#insert_op_debug op dbg rs rd

method insert_op op rs rd =
  self#insert_op_debug op Debuginfo.none rs rd

(* The generic version of this method requires that arch#offset_addressing is
   able to create (reg-immediate) addresses, for small values of immediate.
   That's not possible for SH4, so this version avoids using that type of
   address (but is otherwise identical). Could possibly do better than this. *)

method emit_stores env data regs_addr =
  let a = ref Arch.identity_addressing in
  let clob_regs_addr = Reg.createv typ_int in
  (* We clobber regs_addr, so make a copy *)
  self#insert_moves regs_addr clob_regs_addr;
  ignore(self#insert_op (Iintop_imm(Iadd, -4)) clob_regs_addr clob_regs_addr);
  List.iter
    (fun e ->
      let (op, arg) = self#select_store !a e in
      match self#emit_expr env arg with
        None -> assert false
      | Some regs ->
          begin match op with
	    Istore(_, _) ->
	      for i = 0 to Array.length regs - 1 do
		let r = regs.(i) in
		let kind = if r.typ = Float then Double_u else Word in
		self#insert (Iop(Istore(kind,!a)))
			    (Array.append [|r|] clob_regs_addr) [||];
		a := Arch.offset_addressing !a (size_component r.typ)
	      done
	  | _ ->
	      self#insert (Iop op) (Array.append regs regs_addr) [||];
	      a := Arch.offset_addressing !a (Selectgen.size_expr env e)
	  end)
    data

end

let fundecl f = (new selector)#emit_fundecl f

