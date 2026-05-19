(run NAME) with a bare literal name and (run %{bin:NAME}) record
different deps. The bare-literal form resolves with where=Original_path
(for lang >= 3.14) and records a dep on the build artifact; the pform
form uses the default where=Install_dir and records a dep on the
install staging path.

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

(run %{bin:mybin}) records a dep on the install staging path:

  $ dune rules --format=json _build/default/out-pform \
  >   | jq 'include "dune"; .[] | ruleDepFilePaths' \
  >   | grep mybin
  "_build/install/default/bin/mybin"
