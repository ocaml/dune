%{bin:...} for a workspace executable with (public_name).

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name mypkg))
  > EOF
  $ cat >dune <<'EOF'
  > (executable (public_name mybin) (package mypkg))
  > (rule
  >  (with-stdout-to bin-path
  >   (echo %{bin:mybin})))
  > EOF
  $ cat >mybin.ml <<'EOF'
  > let () = print_endline "hello"
  > EOF

The pform resolves to the build artifact:

  $ dune build bin-path
  $ cat _build/default/bin-path
  mybin.exe

The rule depends on the build artifact:

  $ dune rules --format=json _build/default/bin-path \
  >   | jq 'include "dune"; .[] | ruleDepFilePaths' \
  >   | grep mybin
  "_build/default/mybin.exe"
