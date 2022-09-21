  $ cat > ./dune-project << EOF
  > (lang dune 2.9)
  > (package (name pub))
  > EOF

  $ cat > dune << EOF
  > (library
  >  (public_name pub)
  >  (modules pub)
  >  (libraries priv))
  > 
  > (library
  >  (name priv)
  >  (modules priv)
  >  (package pub)
  >  (libraries dep))
  > 
  > (library
  >  (name dep)
  >  (modules dep))
  > EOF

  $ touch pub.ml priv.ml dep.ml

  $ mkdir bin

  $ cat > ./bin/dune-project << EOF
  > (lang dune 2.9)
  > EOF

  $ cat > ./bin/dune << EOF
  > (executable
  >  (name prog)
  >  (libraries pub))
  > EOF

  $ touch ./bin/prog.ml

  $ dune build 2>&1 | head -n 3
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("Unexpected find result", { found = Not_found; lib.name = "dep" })
