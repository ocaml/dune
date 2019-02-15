#!/bin/bash

# Manipulate output of "dune printenv" to make it independent of the local configuration.

export STANDARD_C_FLAGS=`ocamlc -config | grep ocamlc_cflags | sed 's/ocamlc_cflags: //'`
export STANDARD_CXX_FLAGS=`ocamlc -config | grep ocamlc_cflags | sed 's/ocamlc_cflags: //' | sed 's/-std=[^ ]* //' | sed 's/-std=[^$]*//'`
dune printenv $@ | tr '\n' ' '  | sed 's/ \+/ /g' | sed "s/$STANDARD_C_FLAGS/STANDARD_C_FLAGS/" | sed "s/$STANDARD_CXX_FLAGS/STANDARD_CXX_FLAGS/"
