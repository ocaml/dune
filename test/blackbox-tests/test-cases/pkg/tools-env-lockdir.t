[dune tools env] should also put the binaries installed by the packages of the
lock directory in PATH.

  $ make_lockdir
  $ make_lockpkg provider <<'EOF'
  > (version 0.0.1)
  > (build
  >  (progn
  >   (system "\| cat > helper <<'EOI'
  >           "\| #!/bin/sh
  >           "\| echo from helper
  >           "\| EOI
  >   )
  >   (system "chmod +x helper")
  >   (system "echo 'bin: [ \"helper\" ]' > provider.install")
  >  ))
  > EOF

  $ make_dune_project 3.25
  $ cat >>dune-project <<'EOF'
  > (package (name mypkg) (allow_empty) (dir .) (depends provider))
  > EOF

Build the package so that its binary is installed:

  $ build_pkg provider

  $ eval $(dune tools env)
  $ helper
  from helper
