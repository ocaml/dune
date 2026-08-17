`(deps (package foo))` includes the closure of the libraries installed by
`foo`, but not the closure of `foo`'s package dependencies.

The library closure is necessary because tools consuming an installed OCaml
library expect its transitive library dependencies to be findable. Following
package dependencies would be broader: those dependencies can contain
unrelated executables, data, or libraries, and are not reliably available for
all kinds of packages.

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
  $ mkdir package-only-dep-src
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
closure. This also exercises generated metadata for a nested findlib package.

  $ cat >namespace-src/dune <<EOF
  > (library
  >  (name selected)
  >  (public_name namespace.selected))
  > EOF

  $ cat >namespace-src/selected.ml <<EOF
  > let unused = ()
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
  >  (target redirect)
  >  (deps (package foo))
  >  (action
  >   (with-stdout-to %{target}
  >    (run %{bin:ocamlfind} query bar.old))))
  > (rule
  >  (target namespace-selected)
  >  (deps (package foo))
  >  (action
  >   (with-stdout-to %{target}
  >    (run %{bin:ocamlfind} query namespace.selected))))
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

The package dependency supplies enough of the library closure for external
OCaml tooling to compile and link against `foo`.

  $ dune build main.exe && _build/default/main.exe
  9
  $ dune build stubs-result && cat _build/default/stubs-result
  9
  $ dune build redirect
  $ dune build namespace-selected
  $ dune build ppx-runtime-marker
  File "dune", lines 38-43, characters 0-145:
  38 | (rule
  39 |  (target ppx-runtime-marker)
  40 |  (deps (package test-ppx))
  41 |  (action
  42 |   (with-stdout-to %{target}
  43 |    (run %{bin:ocamlfind} query ppx-runtime))))
  ocamlfind: Package `ppx-runtime' not found
  [1]
  $ dune build root-redirect
  File "dune", lines 44-49, characters 0-162:
  44 | (rule
  45 |  (target root-redirect)
  46 |  (deps (package redirect-root))
  47 |  (action
  48 |   (with-stdout-to %{target}
  49 |    (run %{bin:ocamlfind} query -recursive redirect-root.old))))
  ocamlfind: Package `redirect-target' not found - required by `redirect-root.old'
  [1]

The filtered dune-package files are consumed by a nested Dune invocation. In
particular, Dune must be able to select and link the virtual library's default
implementation from the support package.

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

  $ dune build dune-package-result && cat _build/default/dune-package-result
  51

The layout contains `foo` and the artifacts in its library closure. It does not
contain `package-only-dep`, because that edge exists only in package metadata.

  $ dune rules --format=json _build/default/main.exe |
  > jq_dune '.[] | ruleDepFilePaths' |
  > censor |
  > grep dune-package |
  > sort
  "_build/install/default/.packages/$DIGEST/lib/bar/dune-package"
  "_build/install/default/.packages/$DIGEST/lib/baz/dune-package"
  "_build/install/default/.packages/$DIGEST/lib/foo/dune-package"
  "_build/install/default/.packages/$DIGEST/lib/namespace/dune-package"
  "_build/install/default/.packages/$DIGEST/lib/stubbed/dune-package"

The required libraries' compiled interfaces are tracked by the action.

  $ dune rules --format=json _build/default/main.exe |
  > jq_dune '.[] | ruleDepFilePaths' |
  > censor |
  > grep -E 'lib/(bar/bar|bar/__private__/bar_private/.public_cmi/bar_private|baz/baz)\.cmi' |
  > sort
  "_build/install/default/.packages/$DIGEST/lib/bar/__private__/bar_private/.public_cmi/bar_private.cmi"
  "_build/install/default/.packages/$DIGEST/lib/bar/bar.cmi"
  "_build/install/default/.packages/$DIGEST/lib/baz/baz.cmi"

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
  File "dune", lines 59-64, characters 0-133:
  59 | (rule
  60 |  (target unrelated)
  61 |  (deps (package foo))
  62 |  (action
  63 |   (with-stdout-to %{target}
  64 |    (run %{bin:ocamlfind} query bar.unrelated))))
  ocamlfind: Package `bar.unrelated' not found
  [1]
