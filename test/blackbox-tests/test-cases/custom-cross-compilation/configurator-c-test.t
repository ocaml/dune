Configurator.c_test should use the C compiler from the target context.

  $ unset OCAMLFIND_TOOLCHAIN
  $ unset OCAMLFIND_CONF
  $ external_findlib_path="$(ocamlfind printconf path | tr '\n' ':' | sed 's/:$//')"

  $ actual_ocamlc="$(command -v ocamlc)"
  $ actual_ocamldep="$(command -v ocamldep)"
  $ actual_cc="$(command -v cc)"

The host and target toolchains report different C compilers through
ocamlc -config.

  $ mkdir -p etc/findlib.conf.d
  $ cat >etc/findlib.conf <<EOF
  > path="$external_findlib_path"
  > ocamlc="$PWD/ocamlc-host"
  > EOF
  $ cat >etc/findlib.conf.d/foo.conf <<EOF
  > path(foo)="$external_findlib_path"
  > ocamlc(foo)="$PWD/ocamlc-target"
  > EOF

  $ cat >ocamlc-host <<EOF
  > #!/usr/bin/env sh
  > if [ "\$1" = "-config" ]; then
  >   "$actual_ocamlc" -config | sed 's|^c_compiler:.*|c_compiler: $PWD/cc-host|'
  > else
  >   exec "$actual_ocamlc" "\$@"
  > fi
  > EOF
  $ cat >ocamlc-target <<EOF
  > #!/usr/bin/env sh
  > if [ "\$1" = "-config" ]; then
  >   "$actual_ocamlc" -config | sed 's|^c_compiler:.*|c_compiler: $PWD/cc-target|'
  > else
  >   exec "$actual_ocamlc" "\$@"
  > fi
  > EOF
  $ chmod +x ocamlc-host ocamlc-target

Dune resolves ocamldep next to ocamlc, so it must exist alongside the mock
toolchains. Dependency analysis is toolchain-independent, so it delegates to
the real ocamldep for both contexts.

  $ cat >ocamldep <<EOF
  > #!/usr/bin/env sh
  > exec "$actual_ocamldep" "\$@"
  > EOF
  $ chmod +x ocamldep

The host C compiler always fails. The target C compiler delegates to the real
C compiler.

  $ cat >cc-host <<EOF
  > #!/usr/bin/env sh
  > echo host >> "$PWD/cc-log"
  > exit 1
  > EOF
  $ cat >cc-target <<EOF
  > #!/usr/bin/env sh
  > echo target >> "$PWD/cc-log"
  > exec "$actual_cc" "\$@"
  > EOF
  $ chmod +x cc-host cc-target

  $ export OCAMLFIND_CONF=$PWD/etc/findlib.conf

  $ cat >dune-project <<EOF
  > (lang dune 3.10)
  > EOF
  $ cat >discover.ml <<EOF
  > module C = Configurator.V1
  > 
  > let () =
  >   C.main ~name:"cross-c-compiler" (fun c ->
  >     let selected =
  >       if C.c_test c "int main(void) { return 0; }" then
  >         "target"
  >       else
  >         "host"
  >     in
  >     C.Flags.write_lines "selected-cc" [ selected ])
  > EOF
  $ cat >dune <<EOF
  > (executable
  >  (name discover)
  >  (enabled_if
  >   (= %{context_name} "default"))
  >  (libraries dune.configurator))
  > 
  > (rule
  >  (target selected-cc)
  >  (enabled_if
  >   (= %{context_name} "default.foo"))
  >  (action
  >   (run ./discover.exe)))
  > EOF

  $ build_dir="${DUNE_BUILD_DIR:-_build}"
  $ dune build -x foo selected-cc
  $ cat "$build_dir/default.foo/selected-cc"
  target
  $ cat cc-log
  target
