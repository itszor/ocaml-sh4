#!/bin/sh
# $Id: world.byte.sh,v 1.3 2007/10/08 14:19:34 doligez Exp $
set -e
cd `dirname $0`/..
. build/targets.sh
set -x
$OCAMLBUILD $@ \
  $STDLIB_BYTE $OCAMLC_BYTE $OCAMLLEX_BYTE $OCAMLOPT_BYTE $TOPLEVEL $TOOLS_BYTE \
  $OTHERLIBS_BYTE $OCAMLBUILD_BYTE $DEBUGGER $OCAMLDOC_BYTE $CAMLP4_BYTE
