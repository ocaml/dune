Make sure that dune can handle rules with directory targets that are disabled
with enabled_if.

  $ make_directory_targets_project 3.14

  $ cat > dune <<EOF
  > (rule
  >  (enabled_if false)
  >  (target (dir x))
  >  (action (progn)))
  > EOF

  $ dune build
