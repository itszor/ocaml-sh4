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

(* Only true for 32-bit accesses:
     mov.l @(disp,Rm),Rn
     mov.l Rm,@(disp,Rn)
*)

let is_offset n =
  (n land 3) = 0 && n >= 0 && n < 64

(* Instruction selection *)
class selector = object(self)

inherit Selectgen.selector_generic as super

method is_immediate n =
  n >= 0 && n < 256

method select_addressing = function
    Cop(Cadda, [arg; Cconst_int n]) when is_offset n ->
      (Iindexed n, arg)
  | arg ->
      (Iindirect, arg)

method select_operation op args =
  match op with
    _ -> super#select_operation op args

end

let fundecl f = (new selector)#emit_fundecl f

