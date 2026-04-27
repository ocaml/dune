Test that (deps (package ...)) adds the package's bin/ to PATH.

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name mypkg))
  > EOF
  $ mkdir src
  $ cat >src/dune <<EOF
  > (executable (name mytool) (public_name mytool) (package mypkg) (modules mytool))
  > (library (public_name mypkg) (modules mypkg))
  > EOF
  $ cat >src/mytool.ml <<'EOF'
  > let () = print_endline "hello"
  > EOF
  $ cat >src/mypkg.ml <<'EOF'
  > let x = 1
  > EOF
  $ cat >dune <<'EOF'
  > (rule
  >  (deps (package mypkg))
  >  (action (with-stdout-to out (bash "echo $PATH"))))
  > EOF

  $ baseline=$PATH
  $ dune build out
  $ env_added "$(cat _build/default/out)" "$baseline" | censor
  $PWD/_build/install/default/.packages/$DIGEST/bin
