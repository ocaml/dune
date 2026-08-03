Tests env stanzas in JSOO contexts.

  $ make_dune_project 3.0
  $ cat >dune <<EOF
  > (env (_ (js_of_ocaml (flags :standard "--no-inline"))))
  > (library (name test))
  > EOF
  $ dune printenv --field js_of_ocaml_flags --field js_of_ocaml_link_flags --field js_of_ocaml_build_runtime_flags 2>&1
  (js_of_ocaml_flags
   (--pretty --no-inline))
  (js_of_ocaml_build_runtime_flags (--pretty))
  (js_of_ocaml_link_flags ())
