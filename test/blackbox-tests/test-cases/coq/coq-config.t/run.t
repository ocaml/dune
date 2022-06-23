  $ cat > dune << EOF
  > (rule
  >  (action
  >   (with-outputs-to file
  >    (echo %{coq-config:a}))))
  > EOF

  $ coqc --version
  The Coq Proof Assistant, version 8.15.1
  compiled with OCaml 4.14.0

  $ dune build
  Error: command returned too many lines: /mnt/sda1/.opam/coq.8.15.1/bin/coqc
  --config
  > COQLIB=/mnt/sda1/.opam/coq.8.15.1/lib/coq/
  > COQCORELIB=/mnt/sda1/.opam/coq.8.15.1/lib/coq/../coq-core/
  > DOCDIR=/mnt/sda1/.opam/coq.8.15.1/doc/coq/
  > OCAMLFIND=/mnt/sda1/.opam/coq.8.15.1/bin/ocamlfind
  > CAMLFLAGS=-thread -rectypes -w -a+1..3-4+5..8-9+10..26-27+28..40-41-42+43-44-45+46..47-48+49..57-58+59..66-67-68+69-70   -safe-string -strict-sequence
  > WARN=-warn-error +a-3
  > HASNATDYNLINK=true
  > COQ_SRC_SUBDIRS=boot config lib clib kernel library engine pretyping interp gramlib parsing proofs tactics toplevel printing ide stm vernac plugins/btauto plugins/cc plugins/derive plugins/extraction plugins/firstorder plugins/funind plugins/ltac plugins/ltac2 plugins/micromega plugins/nsatz plugins/ring plugins/rtauto plugins/ssr plugins/ssrmatching plugins/syntax
  > COQ_NATIVE_COMPILER_DEFAULT=yes
  -> required by %{coq-config:a} at dune:4
  -> required by _build/default/file
  -> required by alias all
  -> required by alias default
  [1]
