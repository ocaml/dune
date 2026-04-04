  $ cat >> dune-project <<EOF
  > (lang dune 3.0)
  > EOF

  $ cat >> dune << EOF
  > (library (name a)(modules a))
  > (executable (name main) (modules main) (libraries a))
  > (env (linkall-profile (link_flags (:standard -linkall))))
  > EOF

  $ dune exec ./main.exe
  Starting main

  $ dune exec ./main.exe --profile linkall-profile
  'A' was linked
  Starting main
