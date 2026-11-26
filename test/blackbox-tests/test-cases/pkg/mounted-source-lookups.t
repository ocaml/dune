Rules that look at the sources of the directory they are generated for must
find them when the project is mounted from a lock directory. The workspace
behaviour of each feature is covered by the test named with it.

  $ make_lockdir
  $ make_named_package_project main 3.25 foo
  $ mkdir -p _source/docs/nested _source/stubs/inc _source/escape
  $ (cd _source && make_dune_project_with_package 3.25 foo)
  $ make_dune_source_lockpkg native foo "$PWD/_source"
  $ mount=$pkg_root/foo

A rule that depends on the package asks for everything it installs.

  $ mkdir use
  $ cat > use/dune <<'EOF'
  > (rule
  >  (alias foo)
  >  (deps (package foo))
  >  (action (progn)))
  > EOF

A source tree installed by the package (install/install-source-tree.t). A
layout does not hold installed source trees.

  $ cat > _source/dune <<'EOF'
  > (install
  >  (section share)
  >  (source_trees docs))
  > EOF
  $ echo page > _source/docs/index.txt
  $ echo nested > _source/docs/nested/more.txt
  $ dune build @use/foo 2>&1
  $ find _build/_private/default/.packages -name index.txt

Foreign stubs with an include directory in the source. An opaque consumer
requests the archive cold: the context-wide C compiler probe must not build
that consumer or an unrelated package to find the compiler.

  $ cat > _source/stubs/dune <<'EOF'
  > (library
  >  (public_name foo.stubs)
  >  (name stubs)
  >  (foreign_stubs
  >   (language c)
  >   (names stub)
  >   (include_dirs inc)))
  > EOF
  $ echo '#define VALUE 42' > _source/stubs/inc/value.h
  $ cat > _source/stubs/stub.c <<'EOF'
  > #include <caml/mlvalues.h>
  > #include "value.h"
  > value foo_value(value unit) { return Val_int(VALUE); }
  > EOF
  $ make_lockpkg opaque <<'EOF'
  > (version 1.0)
  > (depends foo)
  > (build
  >  (progn
  >   (system "echo opaque-built")
  >   (system "ls %{pkg:foo:lib}/stubs | grep '_stubs[.]a$'")
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
  libstubs_stubs.a
  $ cat _build/default/result
  ready
  $ dune build $mount/stubs/stubs.cma 2>&1

Duplicate foreign-object diagnostics list every conflicting source. Workspace
paths stay relative to the workspace
(foreign-stubs/duplicate-c-cxx-obj/same-stanza.t).

  $ mkdir _source/stubs/sub
  $ cp _source/stubs/stub.c _source/stubs/sub/stub.c
  $ sed -i '1i (include_subdirs unqualified)' _source/stubs/dune
Mounted diagnostics retain the build context, so the conflicting files can be
opened from the workspace root.

  $ dune build $mount/stubs/stubs.cma 2>&1 | grep '^- '
  - _build/default/.pkg/foo/stubs/stub.c
  - _build/default/.pkg/foo/stubs/sub/stub.c
  [1]
  $ rm _source/stubs/sub/stub.c
  $ dune build $mount/stubs/stubs.cma

An install destination that escapes the package. A workspace project gets a
deprecation warning (stanzas/install/start-install-dst-with-parent-error.t);
for a mounted package it is an error.

  $ cat > _source/escape/dune <<'EOF'
  > (install
  >  (section share)
  >  (files (data as ../escaped)))
  > EOF
  $ echo data > _source/escape/data
  $ dune build @use/foo 2>&1
  File "_build/default/.pkg/foo/escape/dune", line 3, characters 17-27:
  Error: The destination path ../escaped begins with .. which is not allowed.
  Destinations in install stanzas may not begin with .. to prevent a package's
  installed files from escaping that package's install directories.
  [1]

An alias dependency on an alias that is not defined, and on a directory that
does not exist. The workspace messages name the workspace directory; the
mounted messages name the mounted directory's build path.

  $ mkdir _source/aliases
  $ cat > _source/aliases/dune <<'EOF'
  > (rule
  >  (target not-defined)
  >  (deps (alias_rec nothing))
  >  (action (with-stdout-to %{target} (echo ok))))
  > (rule
  >  (target no-directory)
  >  (deps (alias_rec nowhere/default))
  >  (action (with-stdout-to %{target} (echo ok))))
  > EOF
  $ (cd _source && dune build --root . aliases/not-defined 2>&1) | grep 'not defined'
  Alias "nothing" is not defined in aliases or any of its descendants.
  [1]
  $ (cd _source && dune build --root . aliases/no-directory 2>&1) | grep Error:
  Error: Don't know about directory aliases/nowhere!
  [1]
  $ dune build $mount/aliases/not-defined 2>&1
  File "_build/default/.pkg/foo/aliases/dune", line 3, characters 18-25:
  Error: This alias is empty.
  Alias "nothing" is not defined in _build/default/.pkg/foo/aliases or any of
  its descendants.
  [1]
  $ dune build $mount/aliases/no-directory 2>&1
  File "_build/default/.pkg/foo/aliases/dune", line 7, characters 18-33:
  Error: Don't know about directory _build/default/.pkg/foo/aliases/nowhere!
  [1]

A cycle between dynamic includes. The workspace frames name workspace
directories (stanzas/dynamic-include-stanza/cycle.t); the mounted frames name
the build paths.

  $ mkdir -p _source/cyc/a _source/cyc/b
  $ echo '(dynamic_include ../b/dune)' > _source/cyc/a/dune
  $ echo '(dynamic_include ../a/dune)' > _source/cyc/b/dune
  $ dune build @$mount/cyc/a/default 2>&1
  Error: Dependency cycle between:
     dynamic_include _build/default/.pkg/foo/cyc/b/dune in directory
     _build/default/.pkg/foo/cyc/a
  -> dynamic_include _build/default/.pkg/foo/cyc/a/dune in directory
     _build/default/.pkg/foo/cyc/b
  -> dynamic_include _build/default/.pkg/foo/cyc/b/dune in directory
     _build/default/.pkg/foo/cyc/a
  [1]

A public library whose melange runtime dependencies escape its directory. The
workspace hint names the workspace directory
(melange/installed-library-with-runtime-deps-nested-errors.t); the mounted
hint names the build path. The escaping install stanza above is removed so
the package can be installed again.

  $ rm _source/escape/dune
  $ echo '(using melange 1.0)' >> _source/dune-project
  $ mkdir -p _source/mel/src
  $ echo 'function f() {}' > _source/mel/runtime.js
  $ cat > _source/mel/src/dune <<'EOF'
  > (library
  >  (public_name foo.mel)
  >  (name mel)
  >  (modes melange)
  >  (melange.runtime_deps ../runtime.js))
  > EOF
  $ dune build @use/foo 2>&1
  File "_build/default/.pkg/foo/mel/src/dune", line 5, characters 23-36:
  Error: Public library `foo.mel' depends on assets outside its source tree.
  This is not allowed.
  Hint: Move the offending dependency somewhere inside
  `_build/default/.pkg/foo/mel/src'.
  [1]
  $ rm -r _source/mel
  $ sed -i '/using melange/d' _source/dune-project

A documentation stanza naming an mld file that does not exist. The workspace
message names the workspace directory; the mounted message names the build
path.

  $ mkdir _source/manual
  $ echo '(documentation (mld_files missing))' > _source/manual/dune
  $ (cd _source && dune build --root . foo.install 2>&1) | grep Error:
  Error: missing doesn't exist in manual
  [1]
  $ dune build @use/foo 2>&1
  File "_build/default/.pkg/foo/manual/dune", line 1, characters 26-33:
  Error: missing doesn't exist in _build/default/.pkg/foo/manual
  [1]
  $ rm -r _source/manual

A ppx receives the location filename of a dialect-converted module. The driver
below prints the filename it is given and fails. The workspace filename is
relative to the workspace; the mounted filename is the build path.

  $ cat >> _source/dune-project <<'EOF'
  > (dialect
  >  (name mlx)
  >  (implementation (extension mlx) (preprocess (run cat %{input-file})))
  >  (interface (extension mlxi) (preprocess (run cat %{input-file}))))
  > EOF
  $ mkdir -p _source/ppx _source/mlx
  $ cat > _source/ppx/dune <<'EOF'
  > (library
  >  (name showloc_driver)
  >  (modules ())
  >  (ppx.driver
  >   (main "(fun () -> let a = Sys.argv in Array.iteri (fun i x -> if x = \"-loc-filename\" then prerr_endline a.(i + 1)) a; exit 1)")))
  > (library
  >  (name showloc)
  >  (modules ())
  >  (libraries showloc_driver)
  >  (kind ppx_rewriter))
  > EOF
  $ echo '(library (name uses) (preprocess (pps showloc)))' > _source/mlx/dune
  $ echo 'let x = 1' > _source/mlx/uses.mlx
  $ (cd _source && dune build --root . mlx/uses.cma 2>&1) | grep uses.mlx
  mlx/uses.mlx
  [1]
  $ dune build $mount/mlx/uses.cma 2>&1
  File "_build/default/.pkg/foo/mlx/dune", line 1, characters 33-46:
  _build/default/.pkg/foo/mlx/uses.mlx
  [1]

The dune-package of a natively built locked package records, under
--store-orig-source-dir, the directory each library was built from
(dune-package.t). The mounted library's directory is its build directory.

  $ echo '(library (public_name foo))' >> _source/dune
  $ echo 'let x = 1' > _source/foo.ml
  $ dune build --store-orig-source-dir $mount/foo.dune-package 2>&1
  $ grep -A1 orig_src_dir $mount/foo.dune-package
   (orig_src_dir
    $TESTCASE_ROOT/_build/default/.pkg/foo)
  --
   (orig_src_dir
    $TESTCASE_ROOT/_build/default/.pkg/foo/stubs)

A lib-private reference across projects names both projects (lib.t). A
project without packages is named by its directory: the workspace directory
in the workspace, the build directory in a mount.

  $ mkdir -p _source/nested
  $ echo '(lang dune 3.25)' > _source/nested/dune-project
  $ cat > _source/nested/dune <<'EOF'
  > (rule
  >  (alias from-nested)
  >  (action (echo "%{lib-private:foo.stubs:stubs.ml}")))
  > EOF
  $ (cd _source && dune build --root . @nested/from-nested 2>&1) | grep anonymous
  project. The current project's name is "<anonymous nested>", but the
  [1]
  $ dune build @$mount/nested/from-nested 2>&1
  File "_build/default/.pkg/foo/nested/dune", line 3, characters 16-49:
  Error: The variable "lib-private" can only refer to libraries within the same
  project. The current project's name is "<anonymous
  _build/default/.pkg/foo/nested>", but the reference is to "foo".
  [1]

