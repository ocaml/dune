According to the doc: CC is the C compiler command line (list made of the
compiler name followed by its flags) that was used to compile OCaml in the
current build context.

In practice it consists in the concatenation of OCaml's `c_compiler` and flags
The flags are made of the :standard (= ocamlc_cflags) set of ocamlc_flags
merged with (and sometimes replaced by) the flags in the env stanza.

  $ O_CC=$(ocamlc -config-var c_compiler)
  $ O_CCF=$(ocamlc -config-var ocamlc_cflags)

No env
  $ cat > dune <<'EOF'
  > (rule
  >  (alias cc)
  >   (action (echo %{cc})))

  $ dune build @cc | sed "s,${O_CC} ${O_CCF},OK,"
  OK

With added env flags
  $ cat >> dune <<'EOF'
  > (env (_ (c_flags :standard -fPIC)))
  > EOF

  $ dune build @cc | sed "s,${O_CC} ${O_CCF} -fPIC,OK,"
  OK

With redefining env flags
  $ sed -i "s/:standard //g" dune

  $ dune build @cc | sed "s,${O_CC} -fPIC,OK,"
  OK
