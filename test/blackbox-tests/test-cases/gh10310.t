Make sure that dune can handle rules with directory targets that are disabled
with enabled_if.

  $ cat > dune-project <<EOF
  > (lang dune 3.14)
  > (using directory-targets 0.1)
  > EOF

  $ cat > dune <<EOF
  > (rule
  >  (enabled_if false)
  >  (target (dir x))
  >  (action (progn)))
  > EOF

  $ dune build
