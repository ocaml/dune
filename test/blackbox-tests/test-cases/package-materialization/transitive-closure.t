`(deps (package foo))` currently materializes only `foo` and omits the closure
of the libraries installed by it. This snapshots the missing-library errors
seen by OCaml tooling before the closure is materialized.

The intended library closure is narrower than `foo`'s package dependencies:
package dependencies can contain unrelated executables, data, or libraries,
and are not reliably available for all kinds of packages.

  $ make_dune_project 3.24
  $ cat >>dune-project <<EOF
  > (package (name foo) (depends package-only-dep))
  > (package (name bar))
  > (package (name baz))
  > (package (name namespace))
  > (package (name package-only-dep))
  > (package (name ppx-runtime))
  > (package (name redirect-root))
  > (package (name redirect-target))
  > (package (name stubbed))
  > (package (name test-ppx))
  > (package (name virtual-root))
  > (package (name virtual-support))
  > EOF

  $ mkdir foo-src bar-src bar-private-src bar-unrelated-src baz-src namespace-src
  $ mkdir namespace-unrelated-src package-only-dep-src
  $ mkdir redirect-root-src redirect-target-src virtual-root-src
  $ mkdir virtual-support-src virtual-support-impl-src stubbed-src

  $ cat >foo-src/dune <<EOF
  > (library
  >  (public_name foo)
  >  (libraries bar namespace.selected))
  > EOF

  $ cat >foo-src/foo.ml <<EOF
  > let x = Bar.y + 1
  > EOF

  $ cat >bar-src/dune <<EOF
  > (library
  >  (public_name bar)
  >  (libraries baz bar_private stubbed))
  > (deprecated_library_name
  >  (old_public_name bar.old)
  >  (new_public_name bar))
  > EOF

  $ cat >bar-src/bar.ml <<EOF
  > let y = Baz.z + Bar_private.offset + Stubbed.value ()
  > EOF

The installed form of `bar` needs its package-private library too. It is part
of the library closure even though it cannot be named as a public library in
the workspace.

  $ cat >bar-private-src/dune <<EOF
  > (library
  >  (name bar_private)
  >  (package bar))
  > EOF

  $ cat >bar-private-src/bar_private.ml <<EOF
  > let offset = 1
  > EOF

Filtered support metadata preserves properties from a package's META template.

  $ cat >META.bar.template <<EOF
  > support_marker = "kept"
  > # DUNE_GEN
  > EOF

The package that owns `bar` also contains an unrelated library. Requiring
`bar` must not make this sibling library available.

  $ cat >bar-unrelated-src/dune <<EOF
  > (library
  >  (name bar_unrelated)
  >  (public_name bar.unrelated))
  > EOF

  $ cat >bar-unrelated-src/bar_unrelated.ml <<EOF
  > let unused = ()
  > EOF

  $ cat >baz-src/dune <<EOF
  > (library (public_name baz))
  > EOF

  $ cat >baz-src/baz.ml <<EOF
  > let z = 3
  > EOF

Only `namespace.selected`, not a top-level `namespace` library, is in the
closure. Findlib subpackages inherit their directory but not arbitrary
top-level variables, so the filtered META drops `top_marker`.

  $ cat >namespace-src/dune <<EOF
  > (library
  >  (name selected)
  >  (public_name namespace.selected))
  > EOF

  $ cat >namespace-src/selected.ml <<EOF
  > let unused = ()
  > EOF

  $ cat >namespace-unrelated-src/dune <<EOF
  > (library
  >  (name unrelated)
  >  (public_name namespace.unrelated))
  > EOF

  $ cat >namespace-unrelated-src/unrelated.ml <<EOF
  > let unused = ()
  > EOF

  $ cat >META.namespace.template <<EOF
  > top_marker = "drop"
  > # DUNE_GEN
  > EOF

  $ cat >package-only-dep-src/dune <<EOF
  > (library
  >  (name package_only_dep)
  >  (public_name package-only-dep))
  > EOF

  $ cat >package-only-dep-src/package_only_dep.ml <<EOF
  > let unused = ()
  > EOF

Library support includes native stubs and the stublibs entries needed to load
them from bytecode.

  $ cat >stubbed-src/dune <<EOF
  > (library
  >  (public_name stubbed)
  >  (foreign_stubs
  >   (language c)
  >   (names stubbed_stubs)))
  > EOF

  $ cat >stubbed-src/stubbed.ml <<EOF
  > external value : unit -> int = "stubbed_value"
  > EOF

  $ cat >stubbed-src/stubbed_stubs.c <<EOF
  > #include <caml/mlvalues.h>
  > CAMLprim value stubbed_value(value unit)
  > {
  >   (void) unit;
  >   return Val_int(4);
  > }
  > EOF

A deprecated name owned by an explicitly declared package may redirect to a
library in another package. The redirect target is a root of the library
closure even when the declared package has no libraries of its own.

  $ cat >redirect-root-src/dune <<EOF
  > (deprecated_library_name
  >  (old_public_name redirect-root.old)
  >  (new_public_name redirect-target))
  > EOF

  $ cat >redirect-target-src/dune <<EOF
  > (library
  >  (name redirect_target)
  >  (public_name redirect-target))
  > EOF

  $ cat >redirect-target-src/redirect_target.ml <<EOF
  > let value = 42
  > EOF

A virtual library's default implementation must belong to the same package as
the virtual library. It is nevertheless a separate library, and is part of the
link-time library closure selected by a consumer.

  $ cat >virtual-root-src/dune <<EOF
  > (library
  >  (name virtual_root)
  >  (public_name virtual-root)
  >  (libraries virtual-support))
  > EOF

  $ cat >virtual-root-src/virtual_root.ml <<EOF
  > let value = Virtual_support.value
  > EOF

  $ cat >virtual-support-src/dune <<EOF
  > (library
  >  (name virtual_support)
  >  (public_name virtual-support)
  >  (wrapped false)
  >  (virtual_modules virtual_support)
  >  (default_implementation virtual-support.default))
  > EOF

  $ cat >virtual-support-src/virtual_support.mli <<EOF
  > val value : int
  > EOF

  $ cat >virtual-support-impl-src/dune <<EOF
  > (library
  >  (name virtual_support_default)
  >  (public_name virtual-support.default)
  >  (implements virtual-support))
  > EOF

  $ cat >virtual-support-impl-src/virtual_support.ml <<EOF
  > let value = 42
  > EOF

A PPX rewriter's runtime libraries are part of the library support closure
even though they are not ordinary `requires`.

  $ make_hello_ppx_runtime_fixture

  $ cat >hello/dune <<EOF
  > (library
  >  (name hello)
  >  (public_name ppx-runtime))
  > EOF

  $ cat >hello_ppx/dune <<EOF
  > (library
  >  (name hello_ppx)
  >  (public_name test-ppx)
  >  (kind ppx_rewriter)
  >  (ppx_runtime_libraries ppx-runtime)
  >  (ppx.driver (main Hello_ppx.main)))
  > EOF

  $ cat >main.ml <<EOF
  > let () = print_int Foo.x
  > EOF

  $ cat >dune <<'EOF'
  > (rule
  >  (target main.exe)
  >  (deps
  >   main.ml
  >   (package foo))
  >  (action
  >   (run
  >    %{bin:ocamlfind}
  >    ocamlc
  >    -custom
  >    -package
  >    foo
  >    -linkpkg
  >    -o
  >    %{target}
  >    main.ml)))
  > (rule
  >  (targets main.bc stubs-result)
  >  (deps
  >   main.ml
  >   (package foo))
  >  (action
  >   (progn
  >    (run %{bin:ocamlfind} ocamlc -package foo -linkpkg -o main.bc main.ml)
  >    (with-stdout-to stubs-result (run %{bin:ocamlrun} main.bc)))))
  > (rule
  >  (target marker)
  >  (deps (package foo))
  >  (action
  >   (with-stdout-to %{target}
  >    (run %{bin:ocamlfind} query -format "%(support_marker)" bar))))
  > (rule
  >  (target redirect)
  >  (deps (package foo))
  >  (action
  >   (with-stdout-to %{target}
  >    (run %{bin:ocamlfind} query bar.old))))
  > (rule
  >  (target namespace-marker)
  >  (deps (package foo))
  >  (action
  >   (with-stdout-to %{target}
  >    (run %{bin:ocamlfind} query -format "%(top_marker)" namespace))))
  > (rule
  >  (target ppx-runtime-marker)
  >  (deps (package test-ppx))
  >  (action
  >   (with-stdout-to %{target}
  >    (run %{bin:ocamlfind} query ppx-runtime))))
  > (rule
  >  (target root-redirect)
  >  (deps (package redirect-root))
  >  (action
  >   (with-stdout-to %{target}
  >    (run %{bin:ocamlfind} query -recursive redirect-root.old))))
  > EOF

The package dependency does not currently supply the library closure needed
for external OCaml tooling to compile and link against `foo`.

  $ dune build main.exe && _build/default/main.exe
  File "dune", lines 1-16, characters 0-179:
   1 | (rule
   2 |  (target main.exe)
   3 |  (deps
  ....
  14 |    -o
  15 |    %{target}
  16 |    main.ml)))
  ocamlfind: Package `bar' not found - required by `foo'
  [1]
  $ dune build stubs-result && cat _build/default/stubs-result
  File "dune", lines 17-25, characters 0-229:
  17 | (rule
  18 |  (targets main.bc stubs-result)
  19 |  (deps
  20 |   main.ml
  21 |   (package foo))
  22 |  (action
  23 |   (progn
  24 |    (run %{bin:ocamlfind} ocamlc -package foo -linkpkg -o main.bc main.ml)
  25 |    (with-stdout-to stubs-result (run %{bin:ocamlrun} main.bc)))))
  ocamlfind: Package `bar' not found - required by `foo'
  [1]
  $ dune build marker && cat _build/default/marker
  File "dune", lines 26-31, characters 0-148:
  26 | (rule
  27 |  (target marker)
  28 |  (deps (package foo))
  29 |  (action
  30 |   (with-stdout-to %{target}
  31 |    (run %{bin:ocamlfind} query -format "%(support_marker)" bar))))
  ocamlfind: Package `bar' not found
  [1]
  $ dune build redirect
  File "dune", lines 32-37, characters 0-126:
  32 | (rule
  33 |  (target redirect)
  34 |  (deps (package foo))
  35 |  (action
  36 |   (with-stdout-to %{target}
  37 |    (run %{bin:ocamlfind} query bar.old))))
  ocamlfind: Package `bar.old' not found
  [1]
  $ dune build namespace-marker && test -z "$(cat _build/default/namespace-marker)"
  File "dune", lines 38-43, characters 0-160:
  38 | (rule
  39 |  (target namespace-marker)
  40 |  (deps (package foo))
  41 |  (action
  42 |   (with-stdout-to %{target}
  43 |    (run %{bin:ocamlfind} query -format "%(top_marker)" namespace))))
  ocamlfind: Package `namespace' not found
  [1]
  $ dune build ppx-runtime-marker
  File "dune", lines 44-49, characters 0-145:
  44 | (rule
  45 |  (target ppx-runtime-marker)
  46 |  (deps (package test-ppx))
  47 |  (action
  48 |   (with-stdout-to %{target}
  49 |    (run %{bin:ocamlfind} query ppx-runtime))))
  ocamlfind: Package `ppx-runtime' not found
  [1]
  $ dune build root-redirect
  File "dune", lines 50-55, characters 0-162:
  50 | (rule
  51 |  (target root-redirect)
  52 |  (deps (package redirect-root))
  53 |  (action
  54 |   (with-stdout-to %{target}
  55 |    (run %{bin:ocamlfind} query -recursive redirect-root.old))))
  ocamlfind: Package `redirect-target' not found - required by `redirect-root.old'
  [1]

The same missing closure is visible to a nested Dune invocation: the
materialized `foo` metadata names `namespace.selected`, but its package is
absent from the layout.

  $ mkdir consumer
  $ cat >consumer/dune-project <<EOF
  > (lang dune 3.24)
  > EOF

  $ cat >consumer/dune <<EOF
  > (executable
  >  (name main)
  >  (libraries foo virtual-root))
  > EOF

  $ cat >consumer/main.ml <<EOF
  > let () = print_int (Foo.x + Virtual_root.value)
  > EOF

  $ cat >>dune <<'EOF'
  > (rule
  >  (target dune-package-result)
  >  (deps
  >   (package foo)
  >   (package virtual-root)
  >   (source_tree consumer))
  >  (action
  >   (with-stdout-to %{target}
  >    (chdir consumer (run %{bin:dune} exec ./main.exe)))))
  > EOF

  $ dune build dune-package-result 2>&1 | censor
  File "$PWD/_build/install/default/.packages/$DIGEST/lib/foo/dune-package", line 14, characters 15-33:
  14 |  (requires bar namespace.selected)
                      ^^^^^^^^^^^^^^^^^^
  Error: Library "namespace.selected" not found.
  -> required by library "foo" in
     $PWD/_build/install/default/.packages/$DIGEST/lib/foo
  -> required by executable main in dune:2
  -> required by _build/default/.main.eobjs/native/dune__exe__Main.cmx
  -> required by _build/default/main.exe
  [1]

The current layout contains only `foo`. It contains neither the libraries in
its library closure nor `package-only-dep` from package metadata.

  $ dune rules --format=json _build/default/main.exe |
  > jq_dune '.[] | ruleDepFilePaths' |
  > censor |
  > grep dune-package |
  > sort
  "_build/install/default/.packages/$DIGEST/lib/foo/dune-package"

The required libraries' compiled interfaces are consequently not tracked.

  $ dune rules --format=json _build/default/main.exe |
  > jq_dune '.[] | ruleDepFilePaths' |
  > censor |
  > grep -E 'lib/(bar/bar|bar/__private__/bar_private/.public_cmi/bar_private|baz/baz)\.cmi' |
  > sort
  [1]

No artifact belonging to the unrelated sibling is a dependency of the action.

  $ dune rules --format=json _build/default/main.exe |
  > jq_dune '.[] | ruleDepFilePaths' |
  > grep bar_unrelated
  [1]

The unrelated library from package `bar` is not discoverable.

  $ cat >>dune <<'EOF'
  > (rule
  >  (target unrelated)
  >  (deps (package foo))
  >  (action
  >   (with-stdout-to %{target}
  >    (run %{bin:ocamlfind} query bar.unrelated))))
  > EOF

  $ dune build unrelated
  File "dune", lines 65-70, characters 0-133:
  65 | (rule
  66 |  (target unrelated)
  67 |  (deps (package foo))
  68 |  (action
  69 |   (with-stdout-to %{target}
  70 |    (run %{bin:ocamlfind} query bar.unrelated))))
  ocamlfind: Package `bar.unrelated' not found
  [1]
