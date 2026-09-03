A command argument can carry a boolean filter. Some packages write that
filter as an interpolated string rather than as a boolean: (as seen in
`ocamlsdl.0.9.1`)

  $ mkrepo
  $ mkpkg string-filter <<'EOF'
  > build: [
  >   ["echo" "always"]
  >   ["configure" "--with-other=%{lib}%/other" {"%{other:installed}%"}]
  > ]
  > EOF

As the package `other` is not part of the solution, the interpolation expands
to the empty string, which is not a boolean. Opam removes these arguments when
their filter does not evaluate to true. Dune currently crashes:

  $ solve_project <<EOF 2>&1 | head -1
  > (lang dune 3.11)
  > (package
  >  (name x)
  >  (allow_empty)
  >  (depends string-filter))
  > EOF
  Error: exception Invalid_argument("value_bool: \"\"")
  [1]

  $ cat ${default_lock_dir}/string-filter.0.0.1.pkg
  cat: dune.lock/string-filter.0.0.1.pkg: No such file or directory
  [1]
