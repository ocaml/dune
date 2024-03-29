

  $ cat > dune-project <<EOF
  > (lang dune 3.14)
  > (using directory-targets 0.1)
  > EOF

  $ cat > dune <<EOF
  > (rule
  >  (enabled_if false)
  >  (target (dir x))
  >  (action (run mkdir x)))
  > EOF

  $ dune build
