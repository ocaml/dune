An eligible locked Dune project builds in the current graph. The recorded
opaque recipe must not run, and the fetched inputs must not own the artifact
root.

  $ make_lockdir
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name main) (depends foo))
  > EOF
  $ cat > dune <<'EOF'
  > (executable (name main) (libraries foo))
  > EOF
  $ echo 'let () = print_endline Foo.message' > main.ml
  $ mkdir _foo
  $ cat > _foo/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name foo))
  > EOF
  $ echo '(library (public_name foo))' > _foo/dune
  $ echo 'let message = "native"' > _foo/foo.ml
  $ echo unselected > _foo/unused.txt
  $ make_lockpkg foo <<EOF
  > (version 1.0)
  > (source (copy $PWD/_foo))
  > (build (run false))
  > EOF

  $ dune build main.exe
  $ _build/default/main.exe
  native
  $ test -f _build/default/.lockfile/pkg/foo/.foo.objs/native/foo.cmx
  $ test ! -e _build/default/.lockfile/pkg/foo/unused.txt
  $ test ! -e _build/default/.lockfile/pkg/foo/.opam/foo/target
  $ cat _build/_fetch/url/*/dir/unused.txt
  unselected

Native installation uses the shared materializer, with metadata describing the
lock version rather than inventing an opaque recipe output.

  $ dune build @pkg-install
  $ grep '^version' _build/default/.lockfile/pkg/foo/META.foo
  version = "1.0"
  $ test -L _build/_private/default/.packages/*/lib/foo/META
  $ test ! -e _build/default/.lockfile/pkg/foo/.opam/foo/target

An opaque recipe consumes the native package through the shared layout. Its
installed library retains the native provider when resolving its requirements.

  $ make_lockpkg opaque <<'EOF'
  > (version 1.0)
  > (depends foo)
  > (build
  >  (progn
  >   (write-file opaque.ml "let message = Foo.message")
  >   (run ocamlc -I %{pkg:foo:lib} -a -o opaque.cma opaque.ml)
  >   (run ocamlopt -I %{pkg:foo:lib} -a -o opaque.cmxa opaque.ml)
  >   (write-file META "requires = \"foo\"\narchive(byte) = \"opaque.cma\"\narchive(native) = \"opaque.cmxa\"\n")
  >   (write-file opaque.install "lib: [ \"META\" \"opaque.cmi\" \"opaque.cma\" \"opaque.cmx\" \"opaque.cmxa\" \"opaque.a\" ]")))
  > EOF
  $ build_pkg opaque
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name main) (depends opaque))
  > EOF
  $ echo '(executable (name main) (libraries opaque))' > dune
  $ echo 'let () = print_endline Opaque.message' > main.ml
  $ dune build main.exe
  $ _build/default/main.exe
  native

A mounted project's tool lookup is restricted to its locked dependencies even
when its authored package has no exclusive directory. A negative lookup must
not force either unrelated recipes or the package's own installation.

  $ make_lockpkg unrelated <<'EOF'
  > (version 1.0)
  > (build (run false))
  > EOF
  $ cat >> _foo/dune <<'EOF'
  > (rule
  >  (target probe.ml)
  >  (action (write-file %{target} "let visible = %{bin-available:unrelated-tool}")))
  > EOF
  $ echo 'let message = "native/" ^ string_of_bool Probe.visible' > _foo/foo.ml
  $ dune build _build/default/.lockfile/pkg/foo/foo.cma
  $ cat _build/default/.lockfile/pkg/foo/probe.ml
  let visible = false
  $ test ! -e _build/default/.lockfile/pkg/unrelated/.opam/unrelated/target

A second native project can consume that opaque library without searching an
unrelated provider. Its actions use the mounted project root.

  $ mkdir _bar
  $ cat > _bar/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name bar))
  > EOF
  $ cat > _bar/dune <<'EOF'
  > (library (public_name bar) (libraries opaque))
  > (rule
  >  (target generated.ml)
  >  (action
  >   (with-stdout-to %{target}
  >    (run sh -c "test \"$DUNE_PROJECT_ROOT\" = \"$PWD\" && echo 'let suffix = \"/bar\"'"))))
  > EOF
  $ echo 'let message = Opaque.message ^ Generated.suffix' > _bar/bar.ml
  $ make_lockpkg bar <<EOF
  > (version 1.0)
  > (depends opaque)
  > (source (copy $PWD/_bar))
  > (build (run false))
  > EOF
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name main) (dir .) (depends bar))
  > EOF
  $ echo '(executable (name main) (libraries bar))' > dune
  $ echo 'let () = print_endline Bar.message' > main.ml
  $ dune build main.exe
  $ _build/default/main.exe
  native/false/bar
  $ test ! -e _build/default/.lockfile/pkg/unrelated/.opam/unrelated/target

An unused mounted package with a malformed project must not prevent building
another package. Requesting the broken package should report its own error,
without falling back to its otherwise successful recipe.

  $ mkdir _broken
  $ printf '(lang dune 3.25)\n(broken-project)\n' > _broken/dune-project
  $ echo '(library (public_name broken))' > _broken/dune
  $ make_lockpkg broken <<EOF
  > (version 1.0)
  > (source (copy $PWD/_broken))
  > (build (echo opaque-fallback-must-not-run))
  > EOF

  $ dune build main.exe
  $ dune build _build/default/.lockfile/pkg/broken/broken.cma
  File "_build/default/.lockfile/pkg/broken/dune-project", line 2, characters 1-15:
  Error: Unknown field "broken-project"
  [1]

Demand through package materialization also reports the preparation error.
Merely declaring such a rule must not force it.

  $ cat >> dune <<'EOF'
  > (rule
  >  (target needs-broken)
  >  (deps (package broken))
  >  (action (write-file %{target} built)))
  > EOF
  $ dune build main.exe
  $ dune build needs-broken >diagnostic 2>&1
  [1]
  $ grep '^Error:' diagnostic
  Error: Unknown field "broken-project"
  $ test ! -e _build/default/needs-broken

Dune-file decoding errors are deferred as well, not just dune-project errors.

  $ cat > _broken/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name broken))
  > EOF
  $ echo '(broken-stanza)' > _broken/dune
  $ dune build main.exe
  $ dune build _build/default/.lockfile/pkg/broken/broken.cma
  File "_build/default/.lockfile/pkg/broken/dune", line 1, characters 1-14:
  Error: Unknown constructor broken-stanza
  [1]

A library lookup in a consumer of the failed package reports that package's
error rather than claiming its library does not exist.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name main) (dir .) (depends broken))
  > EOF
  $ echo '(executable (name main) (libraries broken))' > dune
  $ cat >> dune <<'EOF'
  > (rule
  >  (target available)
  >  (action (write-file %{target} %{lib-available:broken})))
  > EOF
  $ dune build available
  $ cat _build/default/available
  false
  $ dune build main.exe >diagnostic 2>&1
  [1]
  $ grep '^Error:' diagnostic
  Error: Unknown constructor broken-stanza

Fixing the input makes the package build natively, without running its recipe.

  $ echo '(library (public_name broken))' > _broken/dune
  $ echo 'let message = "repaired"' > _broken/broken.ml
  $ echo 'let () = print_endline Broken.message' > main.ml
  $ dune build main.exe
  $ _build/default/main.exe
  repaired
  $ test ! -e _build/default/.lockfile/pkg/broken/.opam/broken/target

Watch mode preserves the boundary when a previously valid input becomes invalid,
and invalidates the stored failure when the input is repaired.

  $ start_dune
  $ build main.exe
  Success
  $ printf '(lang dune 3.25)\n(broken-project)\n' > _broken/dune-project
  $ build _build/default/.lockfile/pkg/foo/foo.cma
  Success
  $ build main.exe
  Failure
  [1]
  $ cat > _broken/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name broken))
  > EOF
  $ build main.exe
  Success
  $ stop_dune_quiet

Restore the prototype's mounted library-file regression. Like ppxlib's source
generators, a rule reads a source file from another library in its own package
through %{lib:...}. This must not require installing the whole package.

  $ mkdir lib-pform
  $ cd lib-pform
  $ make_lockdir
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name main) (depends foo))
  > EOF
  $ echo '(executable (name main) (libraries foo))' > dune
  $ echo 'let () = print_endline Foo.message' > main.ml
  $ mkdir -p _foo/src _foo/support
  $ cat > _foo/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name foo))
  > EOF
  $ cat > _foo/src/dune <<'EOF'
  > (library (public_name foo))
  > (rule
  >  (target from_lib.ml)
  >  (action (copy %{lib:foo.support:support.ml} %{target})))
  > (rule (target unrelated) (action (run false)))
  > (install (section share) (files unrelated))
  > EOF
  $ echo 'let message = From_lib.message' > _foo/src/foo.ml
  $ cat > _foo/support/dune <<'EOF'
  > (library (name support) (public_name foo.support))
  > EOF
  $ echo 'let message = "mounted library file"' > _foo/support/support.ml
  $ make_lockpkg foo <<EOF
  > (version 1.0)
  > (source (copy $PWD/_foo))
  > (build (run false))
  > EOF

The file resolves to its mounted producer, without demanding unrelated install
entries or the opaque recipe.

  $ dune build main.exe
  $ _build/default/main.exe
  mounted library file
  $ test ! -e _build/default/.lockfile/pkg/foo/src/unrelated
  $ test ! -e _build/default/.lockfile/pkg/foo/.opam/foo/target

Editing the source invalidates the generated file and its consumer.

  $ echo 'let message = "updated library file"' > _foo/support/support.ml
  $ dune build main.exe
  $ _build/default/main.exe
  updated library file

Library-file pforms name installed files, which need not have the same name or
location in the source tree.

  $ echo installed-name > _foo/support/data
  $ cat >> _foo/support/dune <<'EOF'
  > (install (section lib) (files (data as support/renamed.txt)))
  > EOF
  $ cat >> dune <<'EOF'
  > (rule
  >  (target renamed)
  >  (action (copy %{lib:foo.support:renamed.txt} %{target})))
  > EOF
  $ dune build renamed
  $ cat _build/default/renamed
  installed-name
  $ test ! -e _build/default/.lockfile/pkg/foo/src/unrelated

The libexec section shares the library directory and is accessible through
both lib and libexec pforms.

  $ cat >> _foo/support/dune <<'EOF'
  > (install (section libexec) (files (data as support/executable-data)))
  > EOF
  $ cat >> dune <<'EOF'
  > (rule
  >  (target libexec-data)
  >  (action (copy %{libexec:foo.support:executable-data} %{target})))
  > (rule
  >  (target lib-data)
  >  (action (copy %{lib:foo.support:executable-data} %{target})))
  > EOF
  $ dune build libexec-data lib-data
  $ cat _build/default/libexec-data _build/default/lib-data
  installed-name
  installed-name
  $ test ! -e _build/default/.lockfile/pkg/foo/src/unrelated

Public files and binaries must also have ordinary installation rules. A special
pform-only route is not a substitute for these targets.

  $ cat > _foo/support/tool <<'EOF'
  > #!/bin/sh
  > echo native-tool
  > EOF
  $ chmod +x _foo/support/tool
  $ cat >> _foo/support/dune <<'EOF'
  > (install (section bin) (files (tool as foo-tool)))
  > EOF
  $ (cd _foo && dune build --root . \
  >   _build/install/default/lib/foo/support/renamed.txt \
  >   _build/install/default/bin/foo-tool)
  $ _foo/_build/install/default/bin/foo-tool
  native-tool
  $ test ! -e _foo/_build/default/src/unrelated
  $ dune build _build/install/default/lib/foo/support/renamed.txt \
  >   _build/install/default/bin/foo-tool
  $ cat _build/install/default/lib/foo/support/renamed.txt
  installed-name
  $ _build/install/default/bin/foo-tool
  native-tool
  $ test ! -e _build/default/.lockfile/pkg/foo/src/unrelated
  $ test ! -e _build/default/.lockfile/pkg/foo/.opam/foo/target

Generated installation metadata uses the same rules as an ordinary workspace.
Neither a generated opam file nor an odoc configuration requires an opaque build.

  $ mkdir ../install-metadata
  $ cd ../install-metadata
  $ make_dune_project 3.25
  $ mkdir _foo
  $ cat > _foo/dune-project <<'EOF'
  > (lang dune 2.0)
  > (name foo)
  > (version 1.0)
  > (generate_opam_files true)
  > (package (name foo) (depends (doc-helper :with-doc)))
  > EOF
  $ echo '(install (section share) (files data))' > _foo/dune
  $ echo payload > _foo/data
  $ cp -R _foo _workspace
  $ (cd _workspace && dune build --only-packages foo \
  >   foo.opam foo.odoc-config.sexp)
  $ cat _workspace/_build/default/foo.odoc-config.sexp
  (packages doc-helper)
  $ make_lockdir
  $ make_lockpkg foo <<EOF
  > (version 1.0)
  > (source (copy $PWD/_foo))
  > (build (run false))
  > EOF
  $ cat > dune <<'EOF'
  > (rule (target opam) (action (copy %{pkg:foo:lib:opam} %{target})))
  > (rule
  >  (target odoc-config)
  >  (action (copy %{pkg:foo:doc:odoc-config.sexp} %{target})))
  > EOF
  $ dune build opam odoc-config
  $ cmp _workspace/_build/default/foo.opam _build/default/opam
  $ cat _build/default/odoc-config
  (packages doc-helper)
  $ test ! -e _foo/foo.opam
  $ test ! -e _build/default/.lockfile/pkg/foo/.opam/foo/target
  $ dune build --profile release opam
  $ cmp _workspace/_build/default/foo.opam _build/default/opam

The opam-directory layout must also generate the file installed in dev profile,
including when its visible source copy already exists.

  $ cat > _foo/dune-project <<'EOF'
  > (lang dune 3.25)
  > (name foo)
  > (version 1.0)
  > (generate_opam_files true)
  > (opam_file_location inside_opam_directory)
  > (package (name foo) (depends (doc-helper :with-doc)))
  > EOF
  $ mkdir _foo/opam _workspace/opam
  $ echo 'opam-version: "2.0"' > _foo/opam/foo.opam
  $ cp _foo/dune-project _workspace/dune-project
  $ mv _workspace/foo.opam _workspace/opam/foo.opam
  $ (cd _workspace && dune build --only-packages foo \
  >   opam/foo.opam.generated foo.odoc-config.sexp)
  $ dune build opam
  $ cmp _workspace/_build/default/opam/foo.opam.generated _build/default/opam
  $ cat _foo/opam/foo.opam
  opam-version: "2.0"

Release builds use an existing source opam file, as in the workspace. Without
one, the ordinary fallback still generates the installed file.

  $ dune build --profile release opam
  $ cat _build/default/opam
  opam-version: "2.0"
  $ rm _foo/opam/foo.opam
  $ rmdir _foo/opam
  $ dune build --profile release opam
  $ cmp _workspace/_build/default/opam/foo.opam.generated _build/default/opam
  $ test ! -d _foo/opam
  $ test ! -e _build/default/.lockfile/pkg/foo/.opam/foo/target

A context-wide C compiler vendor probe must not build every package to look for
its compiler. In particular, it must not build an opaque consumer while building
that consumer's native foreign library dependency.

  $ mkdir ../foreign-lookup
  $ cd ../foreign-lookup
  $ make_dune_project 3.25
  $ make_lockdir
  $ mkdir _native
  $ cat > _native/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name native))
  > EOF
  $ cat > _native/dune <<'EOF'
  > (library
  >  (public_name native)
  >  (foreign_stubs (language c) (names stubs)))
  > EOF
  $ echo 'external answer : unit -> int = "answer"' > _native/native.ml
  $ cat > _native/stubs.c <<'EOF'
  > #include <caml/mlvalues.h>
  > CAMLprim value answer(value unit) { return Val_int(42); }
  > EOF
  $ make_lockpkg native <<EOF
  > (version 1.0)
  > (source (copy $PWD/_native))
  > (build (run false))
  > EOF
  $ make_lockpkg opaque <<'EOF'
  > (version 1.0)
  > (depends native)
  > (build
  >  (progn
  >   (system "echo opaque-built")
  >   (write-file result ready)
  >   (write-file opaque.install "share: [ \"result\" ]")))
  > EOF
  $ make_lockpkg unrelated <<'EOF'
  > (version 1.0)
  > (build (run false))
  > EOF
  $ cat > dune <<'EOF'
  > (rule (target result) (action (copy %{pkg:opaque:share:result} %{target})))
  > EOF
  $ dune build result
  opaque-built
  $ cat _build/default/result
  ready
  $ test ! -e _build/default/.lockfile/pkg/native/.opam/native/target
  $ test ! -e _build/default/.lockfile/pkg/unrelated/.opam/unrelated/target

A JavaScript executable selects a compiler for its build. Compiling its library
dependencies must not silently switch to an ambient compiler. Fake compilers
make selection observable without depending on a particular JSOO version.

  $ mkdir ../js-compiler-scope
  $ cd ../js-compiler-scope
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (package
  >  (name main)
  >  (dir .)
  >  (depends foo js_of_ocaml-compiler))
  > EOF
  $ echo '(executable (name main) (modes js) (libraries foo))' > dune
  $ echo 'let () = print_int Foo.value' > main.ml
  $ mkdir -p _workspace/foo _workspace/compiler _ambient
  $ cp dune-project dune main.ml _workspace/
  $ cat > _workspace/foo/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name foo))
  > EOF
  $ echo '(library (public_name foo))' > _workspace/foo/dune
  $ echo 'let value = 42' > _workspace/foo/foo.ml
  $ cat > _workspace/compiler/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name js_of_ocaml-compiler))
  > EOF
  $ cat > _workspace/compiler/dune <<'EOF'
  > (executable
  >  (name driver)
  >  (public_name js_of_ocaml)
  >  (package js_of_ocaml-compiler))
  > EOF
  $ cat > _workspace/compiler/driver.ml <<'EOF'
  > let () =
  >   match Array.to_list Sys.argv with
  >   | [_; "--version"] -> print_endline "4.0.0"
  >   | args ->
  >     let rec output = function
  >       | "-o" :: path :: _ ->
  >         let out = open_out path in
  >         output_string out "selected JS compiler\n";
  >         close_out out
  >       | _ :: rest -> output rest
  >       | [] -> exit 1
  >     in
  >     output args
  > EOF
  $ cat > _ambient/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name ambient))
  > EOF
  $ cat > _ambient/dune <<'EOF'
  > (executable (name wrong) (public_name js_of_ocaml) (package ambient))
  > EOF
  $ echo 'let () = prerr_endline "ambient JS compiler"; exit 1' > _ambient/wrong.ml
  $ (cd _ambient && dune build --root . @install)
  $ export PATH="$PWD/_ambient/_build/install/default/bin:$PATH"
  $ (cd _workspace && make_lockdir)
  $ (cd _workspace && dune build --root . main.bc.js)
  $ cat _workspace/_build/default/main.bc.js
  selected JS compiler
  $ make_lockdir
  $ make_lockpkg foo <<EOF
  > (version 1.0)
  > (source (copy $PWD/_workspace/foo))
  > (build (run false))
  > EOF
  $ make_lockpkg js_of_ocaml-compiler <<EOF
  > (version 1.0)
  > (source (copy $PWD/_workspace/compiler))
  > (build (run false))
  > EOF
  $ dune build main.bc.js >output 2>&1
  [1]
  $ grep 'ambient JS compiler' output | sort -u
  ambient JS compiler

Giving the workspace library an explicit package scope exposes the same bug.
The implicit compiler invocation is resolved in the library's scope rather
than the executable's, even though the executable declared the compiler.

  $ cat > _workspace/foo/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name foo) (dir .))
  > EOF
  $ (cd _workspace && dune build --root . main.bc.js) >output 2>&1
  [1]
  $ grep 'ambient JS compiler' output | sort -u
  ambient JS compiler
