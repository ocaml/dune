#!/bin/sh

# Manipulate output of "dune printenv" to make it independent of the local configuration.

STANDARD_C_FLAGS=`ocamlc -config | grep ocamlc_cflags | sed 's/ocamlc_cflags: //' | sed -E 's/ *$//'`
STANDARD_CXX_FLAGS=`ocamlc -config | grep ocamlc_cflags | sed 's/ocamlc_cflags: //' | sed 's/-std=[^ ]* //' | sed 's/-std=[^$]*//' | sed -E 's/ *$//'`
dune printenv $@ | tr -s '\n' ' ' | sed "s/$STANDARD_C_FLAGS/STANDARD_C_FLAGS/" | sed "s/$STANDARD_CXX_FLAGS/STANDARD_CXX_FLAGS/"
