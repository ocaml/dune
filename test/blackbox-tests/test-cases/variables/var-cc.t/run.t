According to the doc: CC is the C compiler command line (list made of the
compiler name followed by its flags) that was used to compile OCaml in the
current build context.

In practice, in dune < 2.8 it consists in the concatenation of OCaml's
`c_compiler` and flags. Theses flags are made of the :standard set of flags
merged with (and sometimes replaced by) the flags in the env stanza.

  $ O_CC=$(ocamlc -config-var c_compiler)
  $ O_CCF=$(ocamlc -config-var ocamlc_cflags)
  $ O_CCPPF=$(ocamlc -config-var ocamlc_cppflags)
  $ O_CC=$(echo $O_CC | sed -e 's/^[ \t]*//')
  $ O_CCF=$(echo $O_CCF | sed -e 's/^[ \t]*//')
  $ O_CCPPF=$(echo $O_CCPPF | sed -e 's/^[ \t]*//')

No env
  $ cat > dune <<'EOF'
  > (rule
  >  (alias cc)
  >   (action (echo %{cc})))
  > EOF

  $ dune build @cc | sed "s,${O_CC} ${O_CCF},OK,"
  OK

With added env flags
  $ cat >> dune <<'EOF'
  > (env (_ (c_flags :standard -fPIC)))
  > EOF

  $ dune build @cc | sed "s,${O_CC} ${O_CCF} -fPIC,OK,"
  OK

With redefining env flags
  $ sed -i.bak "s/:standard //g" dune

  $ dune build @cc | sed "s,${O_CC} -fPIC,OK,"
  OK

Since dune 2.8, when using the use_standard_c_and_cxx_flags option the :standard
set of flag and thus the %{cc} variable contain both the cflags and cppflags
from ocaml config. These flags are not added systematically anymore to the
compiler command line.

  $ cd new_ff_handling

No env
  $ cat > dune <<'EOF'
  > (rule
  >  (alias cc28)
  >   (action (echo %{cc})))
  > EOF

  $ dune build @cc28 | sed "s,${O_CC} ${O_CCF} ${O_CCPPF},OK,"
  OK

With added env flags
  $ cat >> dune <<'EOF'
  > (env (_ (c_flags :standard -fPIC)))
  > EOF

  $ dune build @cc28 | sed "s,${O_CC} ${O_CCF} ${O_CCPPF} -fPIC,OK,"
  OK

With redefining env flags
  $ sed -i.bak "s/:standard //g" dune

  $ dune build @cc28 | sed "s,${O_CC} -fPIC,OK,"
  OK
