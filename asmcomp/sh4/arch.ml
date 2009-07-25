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

(* Specific operations for the SH4 processor *)

open Misc
open Format

(* Machine-specific command-line options *)

let command_line_options = []

(* Addressing modes. No support for @(R0+Rn) addresses for now. *)

type addressing_mode =
    Iindirect				(* reg *)
  | Iindexed of int                     (* reg + displ *)

(* Specific operations *)

type specific_operation = unit

and arith_operation = unit

(* Sizes, endianness *)

let big_endian = false

let size_addr = 4
let size_int = 4
let size_float = 8

(* Operations on addressing modes *)

let identity_addressing = Iindirect

(* Note: this is used to generate reg+offset addresses, where offset is a
   (small) negative integer. We can't support those cases. *)

let offset_addressing addrmode delta =
  match addrmode with
    Iindexed n -> if n >= 0 then Iindexed (n + delta) else assert false
  | Iindirect -> if delta >= 0 then Iindexed (delta) else assert false

let num_args_addressing = function
    Iindexed _ -> 1
  | Iindirect -> 0

(* Printing operations and addressing modes *)

let print_addressing printreg addr ppf arg =
  match addr with
    Iindirect ->
      fprintf ppf "@";
      printreg ppf arg.(0)
  | Iindexed n ->
      fprintf ppf "@(%i," n;
      printreg ppf arg.(0);
      fprintf ppf ")"

let print_specific_operation printreg op ppf arg =
  ()
