We test the `(select)` field of the `(libraries)` field with relative parent
paths

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > EOF

  $ mkdir -p sub
  $ cat > sub/dune <<EOF
  > (library
  >  (name foo)
  >  (libraries
  >   (select ../bar.ml from
  >    (unix -> ../bar.unix.ml)
  >    (!unix -> ../bar.nounix.ml))))
  > EOF

  $ cat > bar.unix.ml <<EOF
  > let () = print_endline "Test: Unix was found!"
  > EOF
  $ cat > bar.nounix.ml <<EOF
  > let () = print_endline "Test: Unix was not found!"
  > EOF

  $ dune build foo.cma 2>&1 | grep "must not crash"
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
