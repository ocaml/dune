Text dump-dot-merlin prints each SUFFIX directive on its own line.

  $ cat > dune-project <<EOF
  > (lang dune 3.24)
  > 
  > (dialect
  >  (name alpha)
  >  (implementation
  >   (extension alpha))
  >  (interface
  >   (extension alphai)))
  > 
  > (dialect
  >  (name beta)
  >  (implementation
  >   (extension beta))
  >  (interface
  >   (extension betai)))
  > EOF
  $ cat > dune <<EOF
  > (executable
  >  (name main))
  > EOF
  $ cat > main.alpha <<EOF
  > let () = print_endline "main"
  > EOF

  $ dune build @check
  $ dune ocaml dump-dot-merlin "$PWD" > merlin.conf
  $ cat merlin.conf | grep '^SUFFIX '
  SUFFIX .alpha .alphai
  SUFFIX .beta .betai
