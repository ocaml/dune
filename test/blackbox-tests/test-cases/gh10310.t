Reproduces a bug where if a rule with a directory target is excluded with
enabled_if then dune crashes.

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

  $ dune build 2>&1 | head -n 7
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("gen_rules returned a set of directory targets that doesn't match the set of directory targets from returned rules",
    { dir = In_build_dir "default"
    ; mismatched_directories =
        map { "default/x" : { message = "not generated"; loc = "dune:1" } }
    })
