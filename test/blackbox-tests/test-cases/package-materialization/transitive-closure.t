`(deps (package foo))` computes the closure of foo's libraries, then includes
all installed files from their owning packages. Libraries in those packages
that are outside the closure do not become additional roots.
Dependencies mentioned only in package metadata are not followed.

  $ make_dune_project 3.24
  $ cat >>dune-project <<EOF
  > (package (name foo) (depends package-only-dep))
  > (package (name bar))
  > (package (name baz))
  > (package (name namespace))
  > (package (name package-only-dep))
  > (package (name ppx-runtime))
  > (package (name ppx-runtime-support))
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

The package that owns `bar` also contains a sibling library. Requiring
`bar` includes the whole package, making this library available too.

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

Requiring `namespace.selected` includes the whole namespace package,
including the original META template and its top-level variables.

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

A PPX rewriter's runtime libraries and their transitive requirements are part
of the library support closure even though they are not ordinary `requires`
of the rewriter.

  $ make_hello_ppx_runtime_fixture

  $ cat >hello/dune <<EOF
  > (library
  >  (name hello)
  >  (public_name ppx-runtime)
  >  (libraries ppx-runtime-support))
  > EOF
  $ mkdir ppx-runtime-support-src
  $ cat >ppx-runtime-support-src/dune <<EOF
  > (library
  >  (name ppx_runtime_support)
  >  (public_name ppx-runtime-support))
  > EOF
  $ touch ppx-runtime-support-src/ppx_runtime_support.ml

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
  >    (run %{bin:ocamlfind} query -recursive ppx-runtime))))
  > (rule
  >  (target root-redirect)
  >  (deps (package redirect-root))
  >  (action
  >   (with-stdout-to %{target}
  >    (run %{bin:ocamlfind} query -recursive redirect-root.old))))
  > EOF

The package dependency supplies the libraries needed by external OCaml
tooling, including stubs, redirects, and PPX runtime libraries.

  $ dune build main.exe && _build/default/main.exe
  9
  $ dune build stubs-result && cat _build/default/stubs-result
  9
  $ dune build redirect
  $ dune build namespace-marker
  $ cat _build/default/namespace-marker
  drop
  $ dune build ppx-runtime-marker
  $ dune build root-redirect

A nested Dune invocation can use the same packages, including a virtual
library and its default implementation.

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

  $ dune build dune-package-result
  $ cat _build/default/dune-package-result
  51

The layout contains the packages reached through library dependencies,
but not `package-only-dep` from package metadata.

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

The required libraries' compiled interfaces are tracked.

  $ dune rules --format=json _build/default/main.exe |
  > jq_dune '.[] | ruleDepFilePaths' |
  > censor |
  > grep -E 'lib/(bar/bar|bar/__private__/bar_private/.public_cmi/bar_private|baz/baz)\.cmi' |
  > sort
  "_build/install/default/.packages/$DIGEST/lib/bar/__private__/bar_private/.public_cmi/bar_private.cmi"
  "_build/install/default/.packages/$DIGEST/lib/bar/bar.cmi"
  "_build/install/default/.packages/$DIGEST/lib/baz/baz.cmi"

The sibling library's interface is also a dependency of the action.

  $ dune rules --format=json main.exe | jq_dune '
  >   rulesMatchingTarget("main.exe") |
  >   ruleHasDepFile("lib/bar/unrelated/bar_unrelated.cmi")'
  true

The sibling library from package `bar` is discoverable.

  $ cat >>dune <<'EOF'
  > (rule
  >  (target unrelated)
  >  (deps (package foo))
  >  (action
  >   (with-stdout-to %{target}
  >    (run %{bin:ocamlfind} query bar.unrelated))))
  > EOF

  $ dune build unrelated

Adding an owning package includes its sibling libraries and non-library files,
but does not add those libraries' dependencies to the closure. Here
bar.unrelated requires sibling-support, which requires bar: that package cycle
must not bring sibling-support into foo's library closure.

  $ echo '(package (name sibling-support))' >>dune-project
  $ mkdir sibling-support-src
  $ cat >sibling-support-src/dune <<'EOF'
  > (library
  >  (name sibling_support)
  >  (public_name sibling-support)
  >  (libraries bar))
  > EOF
  $ echo 'let value = Bar.y' >sibling-support-src/sibling_support.ml
  $ cat >bar-unrelated-src/dune <<'EOF'
  > (library
  >  (name bar_unrelated)
  >  (public_name bar.unrelated)
  >  (libraries sibling-support))
  > EOF
  $ echo 'let value = Sibling_support.value' >bar-unrelated-src/bar_unrelated.ml
  $ echo 'package data' >baz-src/payload
  $ cat >>baz-src/dune <<'EOF'
  > (install (section share) (package baz) (files payload))
  > EOF
  $ cat >dune <<'EOF'
  > (rule
  >  (target package-result)
  >  (deps (package foo))
  >  (action (with-stdout-to %{target} (echo built))))
  > EOF
  $ dune build package-result
  $ dune rules --format=json package-result | jq_dune '
  >   rulesMatchingTarget("package-result") | {
  >     sibling: ruleHasDepFile("lib/bar/unrelated/bar_unrelated.cmi"),
  >     sibling_dependency:
  >       ruleHasDepFile("lib/sibling-support/sibling_support.cmi"),
  >     package_data: ruleHasDepFile("share/baz/payload"),
  >     package_only_dependency:
  >       ruleHasDepFile("lib/package-only-dep/package_only_dep.cmi")
  >   }'
  {
    "sibling": true,
    "sibling_dependency": false,
    "package_data": true,
    "package_only_dependency": false
  }

Explicitly naming bar makes all of its libraries roots, so sibling-support
then belongs to the library closure.

  $ cat >dune <<'EOF'
  > (rule
  >  (target package-result)
  >  (deps (package foo) (package bar))
  >  (action (with-stdout-to %{target} (echo built))))
  > EOF
  $ dune build package-result
  $ dune rules --format=json package-result | jq_dune '
  >   rulesMatchingTarget("package-result") |
  >   ruleHasDepFile("lib/sibling-support/sibling_support.cmi")'
  true
