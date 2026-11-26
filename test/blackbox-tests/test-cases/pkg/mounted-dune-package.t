An eligible locked Dune project builds in the current graph. The recorded
opaque recipe must not run, and the fetched inputs must not own the artifact
root.

  $ make_foo_consumer
  $ mkdir _foo
  $ cat > _foo/dune-project <<'EOF'
  > (lang dune 3.25)
  > (version 2.0)
  > (package (name foo))
  > EOF
  $ cat > _foo/dune <<'EOF'
  > (library (public_name foo))
  > (install (section share_root) (files (foo.el as emacs/site-lisp/foo.el)))
  > (install (section lib_root) (files (META.shim as foo_top/META)))
  > EOF
  $ touch _foo/foo.el _foo/META.shim
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
  $ test -f _build/default/.pkg/foo/.foo.objs/native/foo.cmx
  $ test ! -e _build/default/.pkg/foo/unused.txt

Native installation uses the shared materializer, with metadata describing the
lock version rather than the authored version or an opaque recipe output.

  $ dune build @pkg-install
  $ grep '^version' _build/default/.pkg/foo/META.foo
  version = "1.0"
  $ test -L _build/_private/default/.packages/*/lib/foo/META

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
  $ make_named_package_project main 3.25 opaque
  $ echo '(executable (name main) (libraries opaque))' > dune
  $ echo 'let () = print_endline Opaque.message' > main.ml
  $ dune build main.exe
  $ _build/default/main.exe
  native

A source update to the native library is observed through its opaque consumer.

  $ echo 'let message = "native/false"' > _foo/foo.ml

A second native project can consume that opaque library without searching an
unrelated provider. Its actions use the mounted project root.

  $ make_lockpkg unrelated <<'EOF'
  > (version 1.0)
  > (build (run false))
  > EOF
  $ mkdir _bar
  $ (cd _bar && make_dune_project_with_package 3.25 bar)
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
  $ make_named_package_project main 3.25 bar
  $ echo '(executable (name main) (libraries bar))' > dune
  $ echo 'let () = print_endline Bar.message' > main.ml
  $ dune build main.exe
  $ _build/default/main.exe
  native/false/bar

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
  $ dune build _build/default/.pkg/broken/broken.cma
  File "_build/default/.pkg/broken/dune-project", line 2, characters 1-15:
  Error: Unknown field "broken-project"
  [1]

Dune-file decoding errors are deferred as well; the cases below observe one.

  $ (cd _broken && make_dune_project_with_package 3.25 broken)
  $ echo '(broken-stanza)' > _broken/dune
  $ dune build main.exe

A missing library in a workspace package is reported as missing. A dune
package only defines libraries named after it, so the unrelated native package
that fails to load is not consulted. Remove the unrelated failing opaque
sentinel just for this miss.

  $ mv dune.lock/unrelated.pkg unrelated.pkg
  $ make_named_package_project main 3.25 foo
  $ cat > dune <<'EOF'
  > (executable (name main) (libraries absent))
  > (rule
  >  (target absent-available)
  >  (action (write-file %{target} %{lib-available:absent})))
  > EOF
  $ dune build absent-available
  $ cat _build/default/absent-available
  false
  $ dune build main.exe
  File "dune", line 1, characters 35-41:
  1 | (executable (name main) (libraries absent))
                                         ^^^^^^
  Error: Library "absent" not found.
  -> required by _build/default/.main.eobjs/native/dune__exe__Main.cmx
  -> required by _build/default/main.exe
  [1]
  $ mv unrelated.pkg dune.lock/unrelated.pkg

A library lookup in a consumer of the failed package reports that package's
error rather than claiming its library does not exist. Asking whether the
library is available does too.

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
  File "_build/default/.pkg/broken/dune", line 1, characters 1-14:
  Error: Unknown constructor broken-stanza
  [1]
  $ dune build main.exe
  File "_build/default/.pkg/broken/dune", line 1, characters 1-14:
  Error: Unknown constructor broken-stanza
  [1]

Fixing the input makes the package build natively, without running its recipe.

  $ echo '(library (public_name broken))' > _broken/dune
  $ echo 'let message = "repaired"' > _broken/broken.ml
  $ echo 'let () = print_endline Broken.message' > main.ml
  $ dune build main.exe
  $ _build/default/main.exe
  repaired

Watch mode preserves the boundary when a previously valid input becomes invalid,
and the package loads again when the input is repaired.

  $ start_dune
  $ build main.exe
  Success
  $ printf '(lang dune 3.25)\n(broken-project)\n' > _broken/dune-project
  $ build _build/default/.pkg/foo/foo.cma
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
  $ make_foo_consumer
  $ mkdir -p _foo/src _foo/support
  $ (cd _foo && make_dune_project_with_package 3.25 foo)
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
  $ make_dune_source_lockpkg native foo "$PWD/_foo"

The file resolves to its mounted producer, without demanding unrelated install
entries or the opaque recipe.

  $ dune build main.exe
  $ _build/default/main.exe
  mounted library file

Editing the source invalidates the generated file and its consumer.

  $ echo 'let message = "updated library file"' > _foo/support/support.ml
  $ dune build main.exe
  $ _build/default/main.exe
  updated library file

The package's generated metadata lists the sub-library, which lives in a
subdirectory and is only known by its local name there. A findlib consumer of
the installed package, such as the nested dune of dune's own bootstrap rule,
reads it from this file.

  $ dune build _build/default/.pkg/foo/foo.dune-package _build/default/.pkg/foo/META.foo
  $ grep '^ (name foo' _build/default/.pkg/foo/foo.dune-package
   (name foo)
   (name foo.support)
  $ grep 'package "support"' _build/default/.pkg/foo/META.foo
  package "support" (

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
  $ test ! -e .pkg
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
  $ test ! -e .pkg

Release builds use an existing source opam file, as in the workspace. Without
one, the ordinary fallback still generates the installed file.

  $ dune build --profile release opam
  $ cat _build/default/opam
  opam-version: "2.0"
  $ rm _foo/opam/foo.opam
  $ rmdir _foo/opam
  $ dune build --profile release opam
  $ cmp _workspace/_build/default/opam/foo.opam.generated _build/default/opam
  $ test ! -e .pkg

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
  $ (cd _workspace/foo && make_dune_project_with_package 3.25 foo)
  $ echo '(library (public_name foo))' > _workspace/foo/dune
  $ echo 'let value = 42' > _workspace/foo/foo.ml
  $ (cd _workspace/compiler && make_dune_project_with_package 3.25 js_of_ocaml-compiler)
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
  $ (cd _ambient && make_dune_project_with_package 3.25 ambient)
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
  $ make_dune_source_lockpkg native foo "$PWD/_workspace/foo"
  $ make_dune_source_lockpkg native js_of_ocaml-compiler "$PWD/_workspace/compiler"
  $ dune build main.bc.js
  $ cat _build/default/main.bc.js
  selected JS compiler

An optional linter in a library directory must not make binary discovery
re-enter that library's rules. The package itself is legitimately visible;
finding its declared tools does not require enumerating its library installs.

  $ mkdir ../native-binary-inventory
  $ cd ../native-binary-inventory
  $ mkdir -p _source/src _source/tools
  $ (cd _source && make_dune_project_with_package 3.25 foo)
  $ cat > _source/src/dune <<'EOF'
  > (library (public_name foo))
  > (rule
  >  (alias runtest)
  >  (enabled_if %{bin-available:missing-native-linter})
  >  (action (run false)))
  > (rule
  >  (with-stdout-to available (echo %{bin-available:missing-native-linter})))
  > EOF
  $ echo 'let value = 42' > _source/src/foo.ml
  $ cat > _source/tools/dune <<'EOF'
  > (install (section bin) (files (tool.sh as scoped-native-tool)))
  > EOF
  $ cat > _source/tools/tool.sh <<'EOF'
  > #!/bin/sh
  > echo native tool
  > EOF
  $ chmod +x _source/tools/tool.sh
  $ cat > _source/dune <<'EOF'
  > (rule (with-stdout-to tool-output (run %{bin:scoped-native-tool})))
  > EOF

The ordinary workspace control has the same explicit package scope and an
active, empty lock directory.

  $ cp -R _source _workspace
  $ (cd _workspace &&
  >  make_lockdir && dune build src/foo.cma src/available tool-output)
  $ cat _workspace/_build/default/src/available
  false
  $ cat _workspace/_build/default/tool-output
  native tool

The imported copy uses the same native binary declarations. An unsuccessful
lookup must not fall back to enumerating foo's library installation entries.

  $ make_dune_project 3.25
  $ make_lockdir
  $ make_dune_source_lockpkg native foo "$PWD/_source"
  $ make_lockpkg unrelated <<'EOF'
  > (version 1.0)
  > (build (run false))
  > EOF
  $ dune build $pkg_root/foo/{src/foo.cma,src/available,tool-output}
  $ cat _build/default/.pkg/foo/src/available
  false
  $ cat _build/default/.pkg/foo/tool-output
  native tool

A declared opaque dependency must still be searched when no native declaration
matches. That fallback must not enumerate the native library's installs either.

  $ make_lockpkg provider <<'EOF'
  > (version 1.0)
  > (install
  >  (progn
  >   (write-file %{bin}/scoped-opaque-tool "#!/bin/sh\necho opaque tool")
  >   (run chmod +x %{bin}/scoped-opaque-tool)))
  > EOF
  $ echo '(depends provider)' | append_to_lockpkg foo
  $ cat >> _source/dune <<'EOF'
  > (rule (with-stdout-to opaque-output (run %{bin:scoped-opaque-tool})))
  > EOF
  $ dune build _build/default/.pkg/foo/opaque-output
  $ cat _build/default/.pkg/foo/opaque-output
  opaque tool

Workspace and imported native dependencies belong to one package-set layout.
Opaque dependencies retain their producer prefixes; unrelated native packages
must not contribute singleton layouts to the context environment.

  $ mkdir ../package-set-layout
  $ cd ../package-set-layout
  $ make_lockdir
  $ make_dune_project_with_package 3.25 local
  $ mkdir local
  $ echo '(library (public_name local))' > local/dune
  $ echo 'let value = 1' > local/local.ml
  $ for package in alpha beta unused; do
  >   mkdir "_$package"
  >   printf '(lang dune 3.25)\n(package (name %s))\n' "$package" > "_$package/dune-project"
  >   printf '(library (public_name %s))\n' "$package" > "_$package/dune"
  >   echo 'let value = 2' > "_$package/$package.ml"
  >   make_dune_source_lockpkg native "$package" "$PWD/_$package"
  > done
  $ cat >> _unused/dune <<'EOF'
  > (rule (target unavailable) (action (run false)))
  > (install (section share) (files unavailable))
  > EOF
  $ cat >> dune.lock/alpha.pkg <<'EOF'
  > (exported_env
  >  (= ALPHA_LIB %{pkg-self:lib}))
  > EOF
  $ make_lockpkg opaque <<'EOF'
  > (version 1.0)
  > (install (write-file %{pkg-self:lib}/payload opaque))
  > EOF
  $ cat > dune <<'EOF'
  > (rule
  >  (targets paths libraries)
  >  (deps (package local) (package alpha) (package beta) (package opaque))
  >  (action
  >   (progn
  >    (with-stdout-to paths (run sh -c "test -f \"$ALPHA_LIB/alpha.cmi\" && printf '%s\n' \"$OCAMLPATH\""))
  >    (with-stdout-to libraries (run ocamlfind query alpha beta local)))))
  > (rule
  >  (target reversed)
  >  (deps (package opaque) (package beta) (package alpha) (package local))
  >  (action
  >   (with-stdout-to %{target} (run ocamlfind query alpha beta local))))
  > EOF
  $ dune build paths libraries reversed
  $ cmp _build/default/libraries _build/default/reversed

Unused is not installed:

  $ test ! -e _build/default/.pkg/unused/unused.cma
  $ test -f _build/default/.pkg/opaque/_install/lib/opaque/payload

A nested Dune can consume the same set, and changes to a native dependency
invalidate its output without using an opaque recipe or installing unused.

  $ mkdir consumer
  $ echo '(lang dune 3.25)' > consumer/dune-project
  $ echo '(executable (name main) (libraries alpha beta local))' > consumer/dune
  $ echo 'let () = Printf.printf "%d\n" (Alpha.value + Beta.value + Local.value)' > consumer/main.ml
  $ cat >> dune <<'EOF'
  > (rule
  >  (target nested)
  >  (deps (package alpha) (package local) (package beta) (source_tree consumer))
  >  (action
  >   (with-stdout-to %{target}
  >    (chdir consumer (run %{bin:dune} exec ./main.exe)))))
  > EOF
  $ dune build nested
  $ cat _build/default/nested
  5
  $ echo 'let value = 20' > _beta/beta.ml
  $ dune build nested
  $ cat _build/default/nested
  23
  $ test ! -e _build/default/.pkg/unused/unused.cma

Materialising another native package separately must not make its library
visible through the environment of the original package set.

  $ echo '(library (public_name unused))' > _unused/dune
  $ cat >> dune <<'EOF'
  > (rule
  >  (target installed-unused)
  >  (deps (package unused))
  >  (action (write-file %{target} installed)))
  > (rule
  >  (target unused-visible)
  >  (deps (package local) (package alpha) (package beta))
  >  (action
  >   (with-stdout-to %{target}
  >    (run sh -c "if ocamlfind query unused >/dev/null 2>&1; then echo yes; else echo no; fi"))))
  > EOF
  $ dune build installed-unused
  $ dune build unused-visible
  $ cat _build/default/unused-visible
  no
