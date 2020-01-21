  $ echo "(lang dune 2.2)" > dune-project
  $ cat >dune <<EOF
  > (env (_ (menhir_flags :standard "--comment")))
  > (menhir
  >  (modules parser)
  >  (mode promote))
  > (library (name test))
  > EOF
  $ dune printenv 2>&1 | sed "s/(using menhir .*)/(using menhir <version>)/"
  Info: Appending this line to dune-project: (using menhir <version>)
  
   ((flags
     (-w @1..3@5..28@30..39@43@46..47@49..57@61..62-40 -strict-sequence
       -strict-formats -short-paths -keep-locs))
    (ocamlc_flags (-g))
    (ocamlopt_flags (-g))
    (c_flags
     ($flags))
    (cxx_flags
     ($flags))
    (menhir_flags (--comment)))
  
