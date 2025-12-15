
  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > EOF

  $ cat > t1.t <<'EOF'
  >   $ echo '>> Hello'
  >   >> Hello
  > EOF
  $ dune runtest 2>&1 | grep -i "\(I must not crash\|exn =\)"
     { exn = "Failure(\"lexing: empty token\")" })
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the

