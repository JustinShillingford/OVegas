#!/bin/bash
# DO NOT EDIT THIS FILE

ocamlbuild -use-ocamlfind main.cmo state.cmo command.cmo card.cmo player.cmo table.cmo
if [[ $? -ne 0 ]]; then
  cat <<EOF
===========================================================
WARNING

Your code currently does not compile.  You will receive
little to no credit for submitting this code. Check the
error messages above carefully to determine what is wrong.
See a consultant for help if you cannot determine what is
wrong.
===========================================================
EOF
  exit 1
fi

cat >checktypes.ml <<EOF
(* DO NOT EDIT THIS FILE *)
let check_make_hand : Table.deck -> Table.table * Card.card * Card.card = Table.make_hand
let check_rep_ok : Table.deck -> Table.deck = Table.rep_ok
EOF

if ocamlbuild -use-ocamlfind checktypes.byte ; then
  cat <<EOF
===========================================================
Your function names and types look good to me.
Congratulations!
===========================================================
EOF
else
  cat <<EOF
===========================================================
WARNING

Your function names and types look broken to me.  The code
that you submit might not compile on the grader's machine,
leading to heavy penalties.  Please fix your names and
types.  Check the error messages above carefully to
determine what is wrong.  See a consultant for help if you
cannot determine what is wrong.
===========================================================
EOF
fi
