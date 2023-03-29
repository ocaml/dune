Link-time flags for running cinaps

  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (using cinaps 1.1)
  > EOF

  $ cat > dune <<EOF
  > (cinaps
  >  (files *.ml)
  >  (link_flags -linkall))
  > EOF

  $ touch test.ml

  $ dune build --verbose @cinaps 2>&1 | sed -n 's#.*/cinaps.exe.*\(-linkall\).*#\1#p'
  -linkall
