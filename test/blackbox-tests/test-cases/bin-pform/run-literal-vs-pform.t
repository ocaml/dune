(run NAME) with a bare literal name and (run %{bin:NAME}) both record
a dep on the build artifact. Before wrv only the bare-literal form
did, since the pform default was where=Install_dir; wrv flips the
pform default to Original_path so both paths agree.

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name mypkg))
  > EOF
  $ mkdir src
  $ cat >src/dune <<'EOF'
  > (executable (public_name mybin) (package mypkg))
  > EOF
  $ cat >src/mybin.ml <<'EOF'
  > let () = print_endline "hello"
  > EOF
  $ cat >dune <<'EOF'
  > (rule
  >  (with-stdout-to out-literal
  >   (run mybin)))
  > (rule
  >  (with-stdout-to out-pform
  >   (run %{bin:mybin})))
  > EOF

(run mybin) records a dep on the build artifact:

  $ dune rules --format=json _build/default/out-literal \
  >   | jq 'include "dune"; .[] | ruleDepFilePaths' \
  >   | grep mybin
  "_build/default/src/mybin.exe"

(run %{bin:mybin}) records a dep on the build artifact too:

  $ dune rules --format=json _build/default/out-pform \
  >   | jq 'include "dune"; .[] | ruleDepFilePaths' \
  >   | grep mybin
  "_build/default/src/mybin.exe"
