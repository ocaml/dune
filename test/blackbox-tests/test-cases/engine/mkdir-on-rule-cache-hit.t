  $ cat > dune-project <<'EOF'
  > (lang dune 3.20)
  > EOF
  $ mkdir sub
  $ cat > sub/dune <<'EOF'
  > (rule
  >  (target a)
  >  (action (write-file %{target} a)))
  > (rule
  >  (target b)
  >  (action (write-file %{target} b)))
  > EOF

Warm the workspace-local rule cache.

  $ dune build sub/a sub/b

A null build tries to create the common target directory once for each rule,
even though both rules hit the workspace-local cache.

  $ strace -e trace=mkdir,mkdirat -o trace dune build sub/a sub/b
  $ grep -c 'mkdir.*"_build/default/sub"' trace || true
  2
