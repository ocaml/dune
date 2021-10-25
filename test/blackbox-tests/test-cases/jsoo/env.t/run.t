  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF
  $ cat >dune <<EOF
  > (env (_ (js_of_ocaml (flags :standard "--no-inline"))))
  > (library (name test))
  > EOF
  $ dune printenv --field js_of_ocaml_flags --field js_of_ocaml_link_flags --field js_of_ocaml_build_runtime_flags 2>&1
  (js_of_ocaml_flags
   (--pretty --source-map-inline --no-inline))
  (js_of_ocaml_build_runtime_flags
   (--pretty --source-map-inline))
  (js_of_ocaml_link_flags (--source-map-inline))
