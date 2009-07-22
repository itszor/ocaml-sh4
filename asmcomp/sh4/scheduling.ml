(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Mach

(* Instruction scheduling for the SH4 *)

class scheduler = object

inherit Schedgen.scheduler_generic

method oper_latency = function
  _ -> 1

method oper_issue_cycles = function
  _ -> 1

end

let fundecl f = (new scheduler)#schedule_fundecl f
