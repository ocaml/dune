Promotion repairs executable permissions even when the promoted file already has
the right contents.

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

  $ cat > dune <<'EOF'
  > (rule
  >  (targets script)
  >  (mode promote)
  >  (action
  >   (bash "printf '#!/bin/sh\necho hi\n' > script; chmod +x script")))
  > EOF

  $ dune build script
  $ test -x script && echo executable || echo not-executable
  executable

Simulate an external edit that leaves the promoted file contents unchanged but
removes the executable bit.

  $ chmod -x script
  $ cat script
  #!/bin/sh
  echo hi

  $ dune build script
  $ test -x script && echo executable || echo not-executable
  not-executable
