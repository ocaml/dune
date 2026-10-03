The same authored project uses the same directory rules as a workspace project
and as a mounted dependency. Include-subdirs groups, source-only directories and
cram fixtures all retain their source view.

  $ make_lockdir
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name main) (depends foo))
  > EOF
  $ mkdir -p _source/source-only _source/group/sub _source/fixture.t/empty
  $ cat > _source/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name foo) (dir .))
  > EOF
  $ cat > _source/dune <<'EOF'
  > (library (public_name foo))
  > (rule
  >  (target verified)
  >  (deps foo.cma group/group.cma
  >        (alias source-only/basic.t)
  >        (alias group/sub/group.t)
  >        (alias fixture.t))
  >  (action (write-file %{target} verified)))
  > EOF
  $ echo 'let value = 1' > _source/foo.ml
  $ cat > _source/group/dune <<'EOF'
  > (include_subdirs qualified)
  > (library (name group) (public_name foo.group))
  > EOF
  $ echo 'let value = 2' > _source/group/sub/value.ml
  $ cat > _source/source-only/basic.t <<'EOF'
  >   $ echo source-only
  >   source-only
  > EOF
  $ cat > _source/group/sub/group.t <<'EOF'
  >   $ echo group
  >   group
  > EOF
  $ cat > _source/fixture.t/run.t <<'EOF'
  >   $ cat payload
  >   fixture
  >   $ test -d empty
  > EOF
  $ echo fixture > _source/fixture.t/payload
  $ echo '(' > _source/fixture.t/dune-project

Run the project normally first. The malformed dune-project inside the cram
fixture is data, not project configuration.

  $ (cd _source && dune build --root . --only-packages foo verified)
  $ cat _source/_build/default/verified
  verified

Mount the identical source. The lock recipe is deliberately unusable, so the
same rules must be generated in the current graph. Workspace package selection
and inherited cram stanzas must not replace the mounted project's configuration.

  $ cat > dune <<'EOF'
  > (cram
  >  (applies_to :whole_subtree)
  >  (deps missing-workspace-only-dependency))
  > EOF

  $ make_lockpkg foo <<EOF
  > (version 1.0)
  > (source (copy $PWD/_source))
  > (build (run false))
  > EOF
  $ dune build --only-packages main _build/default/.lockfile/pkg/foo/verified
  $ cat _build/default/.lockfile/pkg/foo/verified
  verified
  $ test ! -e _build/default/.lockfile/pkg/foo/.opam/foo/target

A selected project can be nested within its source snapshot. Its ordinary
private PPX belongs to that same project; no other package owns the driver.
Internal outputs must use the build partition, not the nested source root.

  $ mkdir -p _source/nested/driver
  $ cat > _source/nested/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name bar) (dir .))
  > EOF
  $ cat > _source/nested/dune <<'EOF'
  > (library (public_name bar) (preprocess (staged_pps identity)))
  > EOF
  $ echo 'let value = 3' > _source/nested/bar.ml
  $ cat > _source/nested/driver/dune <<'EOF'
  > (library
  >  (name identity)
  >  (kind ppx_rewriter)
  >  (ppx.driver (main Identity.main)))
  > EOF
  $ cat > _source/nested/driver/identity.ml <<'EOF'
  > let main () =
  >   let input = open_in_bin Sys.argv.(Array.length Sys.argv - 2) in
  >   let contents = really_input_string input (in_channel_length input) in
  >   close_in input;
  >   let output = open_out_bin Sys.argv.(Array.length Sys.argv - 1) in
  >   output_string output contents;
  >   close_out output
  > EOF
  $ (cd _source && dune build --root . --only-packages bar nested/bar.cma)
  $ make_lockpkg bar <<EOF
  > (version 1.0)
  > (source (copy $PWD/_source))
  > (build (run false))
  > EOF

  $ dune build _build/default/.lockfile/pkg/bar/nested/bar.cma
  $ test -f _build/default/.lockfile/pkg/bar/nested/bar.cma
  $ find _build/default/.lockfile/pkg/bar/.ppx -name ppx.exe | wc -l
  1
  $ test ! -e _build/default/.lockfile/pkg/bar/nested/.ppx

With distinct host and target contexts, both builds put the driver in the host
partition while compiling the consuming library in the target context.

  $ cat > _source/dune-workspace <<'EOF'
  > (lang dune 3.25)
  > (context (default))
  > (context (default (name host)))
  > (context (default (name target) (host host)))
  > EOF
  $ (cd _source && dune build --root . --only-packages bar _build/target/nested/bar.cma)
  $ find _source/_build/host/.ppx -name ppx.exe | wc -l
  1
  $ test ! -e _source/_build/target/.ppx
  $ cat > dune-workspace <<'EOF'
  > (lang dune 3.25)
  > (pkg enabled)
  > (context (default))
  > (context (default (name host)))
  > (context (default (name target) (host host)))
  > EOF
  $ dune build _build/target/.lockfile/pkg/bar/nested/bar.cma
  $ test -f _build/target/.lockfile/pkg/bar/nested/bar.cma
  $ find _build/host/.lockfile/pkg/bar/.ppx -name ppx.exe | wc -l
  1
  $ test ! -e _build/target/.lockfile/pkg/bar/.ppx

A workspace does not need a context called default. Shared lock storage is not
itself a workspace build context.

  $ cat > dune-workspace <<'EOF'
  > (lang dune 3.25)
  > (pkg enabled)
  > (context (default (name host)))
  > (context (default (name target) (host host)))
  > EOF

  $ echo 'let value = 4' > _source/nested/bar.ml
  $ dune build _build/target/.lockfile/pkg/bar/nested/bar.cma
  $ test -d _build/_private/default/.lock/dune.lock
  $ test -f _build/target/.lockfile/pkg/bar/nested/bar.cma
  $ find _build/host/.lockfile/pkg/bar/.ppx -name ppx.exe | wc -l
  1

Recursive aliases follow authored directory selection. They visit ordinary
children, but do not enter ignored or explicitly vendored directories.

  $ mkdir -p _source/checks/child _source/checks/ignored _source/checks/vendored
  $ cat > _source/checks/dune <<'EOF'
  > (dirs :standard \ ignored)
  > (vendored_dirs vendored)
  > EOF
  $ cat > _source/checks/child/dune <<'EOF'
  > (rule (alias scan) (action (echo scanned)))
  > EOF
  $ for dir in ignored vendored; do
  >   echo '(rule (alias scan) (action (run false)))' > _source/checks/$dir/dune
  > done
  $ cat >> _source/dune <<'EOF'
  > (rule
  >  (target traversed)
  >  (deps (alias_rec checks/scan))
  >  (action (write-file %{target} traversed)))
  > EOF
  $ (cd _source && dune build --root . --only-packages foo _build/host/traversed)
  scanned

  $ dune build _build/host/.lockfile/pkg/foo/traversed
  scanned
  $ cat _build/host/.lockfile/pkg/foo/traversed
  traversed

The same recursive alias can also be requested directly on the command line.

  $ (cd _source && dune build --root . --only-packages foo @_build/host/checks/scan)
  $ dune build @_build/host/.lockfile/pkg/foo/checks/scan

Implicit default aliases use the same source view and include ordinary targets.

  $ cat >> _source/checks/child/dune <<'EOF'
  > (rule (target implicit) (action (write-file %{target} implicit)))
  > EOF
  $ (cd _source && dune build --root . --only-packages foo @_build/host/checks/child/default)
  $ cat _source/_build/host/checks/child/implicit
  implicit
  $ dune build @_build/host/.lockfile/pkg/foo/checks/child/default
  $ cat _build/host/.lockfile/pkg/foo/checks/child/implicit
  implicit

Workspace alias traversal must not sweep in dependency mounts.

  $ dune build @_build/host/scan
  Error: Alias "scan" specified on the command line is empty.
  It is not defined in . or any of its descendants.
  [1]

Requesting a source directory is shorthand for its default alias in either view.

  $ cat >> _source/checks/child/dune <<'EOF'
  > (rule (target direct) (action (write-file %{target} directory)))
  > EOF
  $ (cd _source && dune build --root . --only-packages foo _build/host/checks/child)
  $ cat _source/_build/host/checks/child/direct
  directory

  $ dune build _build/host/.lockfile/pkg/foo/checks/child
  $ cat _build/host/.lockfile/pkg/foo/checks/child/direct
  directory

Test discovery uses the same view for direct cram requests and directories.

  $ (cd _source && dune runtest --root . _build/host/source-only/basic.t)
  $ dune runtest _build/host/.lockfile/pkg/foo/source-only/basic.t
  $ (cd _source && dune runtest --root . _build/host/source-only)
  $ dune runtest _build/host/.lockfile/pkg/foo/source-only
  $ (cd _source && dune runtest --root . _build/host/fixture.t)
  $ dune runtest _build/host/.lockfile/pkg/foo/fixture.t

OCaml test entry points continue to resolve through the same directory contents.

  $ mkdir _source/unit
  $ cat > _source/unit/dune <<'EOF'
  > (test (name smoke) (libraries foo))
  > EOF
  $ echo 'let () = print_endline (if Foo.value = 1 then "unit" else "bad")' > _source/unit/smoke.ml
  $ (cd _source && dune runtest --root . _build/host/unit/smoke.ml)
  unit
  $ dune runtest _build/host/.lockfile/pkg/foo/unit/smoke.ml
  unit

Copy-files globs include generated files as well as authored sources, including
when the input directory is a descendant of the copying directory.

  $ mkdir -p _source/copies/input
  $ echo '(copy_files input/*.txt)' > _source/copies/dune
  $ echo authored > _source/copies/input/source.txt
  $ cat > _source/copies/input/dune <<'EOF'
  > (rule (target generated.txt) (action (write-file %{target} generated)))
  > EOF
  $ (cd _source && dune build --root . --only-packages foo _build/host/copies/source.txt _build/host/copies/generated.txt)
  $ cat _source/_build/host/copies/source.txt _source/_build/host/copies/generated.txt
  authored
  generated
  $ dune build _build/host/.lockfile/pkg/foo/copies/source.txt
  $ cat _build/host/.lockfile/pkg/foo/copies/source.txt
  authored

  $ dune build _build/host/.lockfile/pkg/foo/copies/generated.txt
  $ cat _build/host/.lockfile/pkg/foo/copies/generated.txt
  generated

Only-sources globs exclude generated inputs and remove stale copied targets.
Adding a source file updates the glob in both views.

  $ echo '(copy_files (only_sources) (files input/*.txt))' > _source/copies/dune
  $ (cd _source && dune build --root . --only-packages foo _build/host/copies/source.txt)
  $ dune build _build/host/.lockfile/pkg/foo/copies/source.txt
  $ test ! -e _source/_build/host/copies/generated.txt
  $ test ! -e _build/host/.lockfile/pkg/foo/copies/generated.txt
  $ echo added > _source/copies/input/added.txt
  $ (cd _source && dune build --root . --only-packages foo _build/host/copies/added.txt)
  $ cat _source/_build/host/copies/added.txt
  added
  $ dune build _build/host/.lockfile/pkg/foo/copies/added.txt
  $ cat _build/host/.lockfile/pkg/foo/copies/added.txt
  added

Declared source children survive parent cleanup. Once the source directory is
removed, its stale output directory must no longer be retained.

  $ test -f _source/_build/host/copies/input/source.txt
  $ test -f _build/host/.lockfile/pkg/foo/copies/input/source.txt
  $ echo '(rule (target kept) (action (write-file %{target} kept)))' > _source/copies/dune
  $ rm -r _source/copies/input
  $ (cd _source && dune build --root . --only-packages foo _build/host/copies/kept)
  $ dune build _build/host/.lockfile/pkg/foo/copies/kept
  $ test ! -e _source/_build/host/copies/input
  $ test ! -e _build/host/.lockfile/pkg/foo/copies/input

Generated directory targets remain distinct from authored directories. Copies
from a sibling's directory target work in either view.

  $ mkdir _source/dir-producer _source/dir-consumer
  $ cat > _source/dir-producer/dune <<'EOF'
  > (rule
  >  (target (dir files))
  >  (deps (sandbox always))
  >  (action (system "mkdir files; echo directory > files/generated.txt")))
  > EOF
  $ echo '(copy_files ../dir-producer/files/*.txt)' > _source/dir-consumer/dune
  $ (cd _source && dune build --root . --only-packages foo _build/host/dir-consumer/generated.txt)
  $ cat _source/_build/host/dir-consumer/generated.txt
  directory
  $ dune build _build/host/.lockfile/pkg/foo/dir-consumer/generated.txt
  $ cat _build/host/.lockfile/pkg/foo/dir-consumer/generated.txt
  directory

Declaring a directory target does not give it authored-source independence.
Copying from one's own directory target still produces the ordinary cycle.

  $ mkdir _source/cycle
  $ cat > _source/cycle/dune <<'EOF'
  > (rule
  >  (target (dir files))
  >  (deps (sandbox always))
  >  (action (system "mkdir files; echo directory > files/generated.txt")))
  > (copy_files files/*.txt)
  > EOF
  $ (cd _source && dune build --root . --only-packages foo _build/host/cycle/generated.txt) > ordinary-error 2>&1
  [1]
  $ grep '^Error:' ordinary-error
  Error: Dependency cycle between:
  $ dune build _build/host/.lockfile/pkg/foo/cycle/generated.txt > mounted-error 2>&1
  [1]
  $ grep '^Error:' mounted-error
  Error: Dependency cycle between:

Promoted directory targets can replace authored directories in the build tree.
A mounted build must retain the source mask without promoting into its inputs.

  $ mkdir -p _source/promoted/dir
  $ echo authored > _source/promoted/dir/source.txt
  $ cat > _source/promoted/dune <<'EOF'
  > (rule
  >  (mode promote)
  >  (target (dir dir))
  >  (deps (sandbox always))
  >  (action (system "mkdir dir; echo promoted > dir/generated.txt")))
  > EOF
  $ dune build _build/host/.lockfile/pkg/foo/promoted/dir/generated.txt
  $ cat _build/host/.lockfile/pkg/foo/promoted/dir/generated.txt
  promoted
  $ test -f _source/promoted/dir/source.txt
  $ test ! -e _source/promoted/dir/generated.txt
  $ (cd _source && dune build --root . --only-packages foo _build/host/promoted/dir/generated.txt)
  $ cat _source/_build/host/promoted/dir/generated.txt
  promoted

Mounted dependencies follow the vendored warning policy, not the workspace's
warning-as-error policy. Compare the same project in all three roles. An
unusable lock recipe ensures the mounted build uses ordinary native rules.

  $ mkdir warning-policy
  $ cd warning-policy
  $ cat > dune-workspace <<'EOF'
  > (lang dune 3.25)
  > (pkg enabled)
  > EOF
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name main) (depends dep))
  > EOF
  $ mkdir -p _source/warning _source/alert _source/modules
  $ cat > _source/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name dep))
  > EOF
  $ cat > _source/warning/dune <<'EOF'
  > (library (name warning) (public_name dep.warning))
  > EOF
  $ echo 'let value = let unused = 0 in 42' > _source/warning/warning.ml
  $ cat > _source/alert/dune <<'EOF'
  > (library
  >  (name alert)
  >  (public_name dep.alert)
  >  (flags (:standard -alert @deprecated)))
  > EOF
  $ cat > _source/alert/alert.ml <<'EOF'
  > module Old : sig
  >   val value : int [@@deprecated "Use another value"]
  > end = struct
  >   let value = 42
  > end
  > let value = Old.value
  > EOF
  $ cat > _source/modules/dune <<'EOF'
  > (library
  >  (name modules)
  >  (public_name dep.modules)
  >  (modules modules)
  >  (private_modules ignored))
  > EOF
  $ echo 'let value = 42' > _source/modules/modules.ml
  $ echo 'let value = 0' > _source/modules/ignored.ml
  $ mkdir _vendor
  $ cp -R _source _vendor/dep
  $ echo '(lang dune 3.25)' > _vendor/dune-project
  $ echo '(vendored_dirs dep)' > _vendor/dune

The development workspace rejects these warnings and module declarations.

  $ (cd _source && dune build --root . warning/warning.cma) 2>workspace-warning
  [1]
  $ grep '^Error' workspace-warning
  Error (warning 26 [unused-var]): unused variable unused.
  $ (cd _source && dune build --root . alert/alert.cma) 2>workspace-alert
  [1]
  $ grep '^Error' workspace-alert
  Error (alert deprecated): Old.value
  $ (cd _source && dune build --root . modules/modules.cma) 2>workspace-modules
  [1]
  $ grep '^Error' workspace-modules
  Error: These modules appear in the private_modules field:

Vendoring suppresses warnings and alerts, including explicitly enabled alerts,
and tolerates private modules omitted from the modules field.

  $ (cd _vendor && dune build --root . dep/warning/warning.cma \
  >   dep/alert/alert.cma dep/modules/modules.cma)

The mounted project receives the same treatment, with no warnings or alerts.

  $ make_lockdir
  $ make_lockpkg dep <<EOF
  > (version 1.0)
  > (source (copy $PWD/_source))
  > (build (run false))
  > EOF
  $ mounted=_build/default/.lockfile/pkg/dep
  $ dune build "$mounted/warning/warning.cma"
  $ dune build "$mounted/alert/alert.cma"
  $ dune build "$mounted/modules/modules.cma"
  $ test ! -e "$mounted/.opam/dep/target"

Older projects use a different set of default development warning flags, but
should still receive the vendored override when mounted.

  $ cat > _source/dune-project <<'EOF'
  > (lang dune 3.20)
  > (package (name dep))
  > EOF
  $ dune build "$mounted/warning/warning.cma"

Neither vendoring nor mounting may suppress actual type errors.

  $ echo 'let value : int = "wrong"' > _source/warning/warning.ml
  $ cp _source/warning/warning.ml _vendor/dep/warning/warning.ml
  $ (cd _vendor && dune build --root . dep/warning/warning.cma) 2>vendor-type
  [1]
  $ grep -q '^Error:' vendor-type
  $ dune build "$mounted/warning/warning.cma" 2>mounted-type
  [1]
  $ grep -q '^Error:' mounted-type

Recursive globs can reach a sibling directory, even above the current project
root. Rule dependencies and install entries must traverse the same input tree.

  $ mkdir ../relative-globs
  $ cd ../relative-globs
  $ make_dune_project 3.25
  $ make_lockdir
  $ mkdir -p _source/project/sub _source/data/nested
  $ echo '(lang dune 3.25)' > _source/dune-project
  $ cat > _source/project/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name foo))
  > EOF
  $ echo first > _source/data/first.txt
  $ echo second > _source/data/nested/second.txt
  $ cat > _source/project/sub/dune <<'EOF'
  > (rule
  >  (target result)
  >  (deps (glob_files_rec ../../data/*.txt))
  >  (action (with-stdout-to %{target} (cat %{deps}))))
  > (install
  >  (package foo)
  >  (section share)
  >  (files (glob_files_rec (../../data/*.txt with_prefix data))))
  > EOF
  $ (cd _source && dune build --root . project/sub/result project/foo.install)
  $ cat _source/_build/default/project/sub/result
  first
  second
  $ make_lockpkg foo <<EOF
  > (version 1.0)
  > (source (copy $PWD/_source))
  > (build (run false))
  > EOF
  $ cat > dune <<'EOF'
  > (rule
  >  (target installed)
  >  (action (copy %{pkg:foo:share:data/nested/second.txt} %{target})))
  > EOF
  $ dune build _build/default/.lockfile/pkg/foo/project/sub/result installed
  $ cat _build/default/.lockfile/pkg/foo/project/sub/result
  first
  second
  $ cat _build/default/installed
  second
  $ test ! -e _build/default/.lockfile/pkg/foo/.opam/foo/target

Changes to sibling inputs invalidate both uses of the glob.

  $ echo changed > _source/data/nested/second.txt
  $ dune build _build/default/.lockfile/pkg/foo/project/sub/result installed
  $ cat _build/default/.lockfile/pkg/foo/project/sub/result
  first
  changed
  $ cat _build/default/installed
  changed

Project-local installation targets and aliases are ordinary rules too. Selecting
one package must not create installation targets for its unselected siblings.

  $ cat >> _source/project/dune-project <<'EOF'
  > (package (name omitted))
  > EOF
  $ cat > _source/project/dune <<'EOF'
  > (library (public_name foo))
  > (env (_ (binaries (index-tool as ocaml-index))))
  > EOF
  $ echo 'let value = 42' > _source/project/foo.ml
  $ cat > _source/project/index-tool <<'EOF'
  > #!/bin/sh
  > test "$1" = aggregate || exit 1
  > shift
  > test "$1" = -o || exit 1
  > touch "$2"
  > EOF
  $ chmod +x _source/project/index-tool
  $ (cd _source && dune build --root . --only-packages foo \
  >   project/foo.install @project/install @project/ocaml-index)
  $ test -f _source/_build/default/project/.foo.objs/cctx.ocaml-index
  $ test ! -e _source/_build/default/project/omitted.install

The mounted aliases build the same project targets without running the recipe.

  $ dune build @_build/default/.lockfile/pkg/foo/project/install
  $ test -f _build/default/.lockfile/pkg/foo/project/foo.install
  $ dune build @_build/default/.lockfile/pkg/foo/project/ocaml-index
  $ test -f _build/default/.lockfile/pkg/foo/project/.foo.objs/cctx.ocaml-index
  $ dune build _build/default/.lockfile/pkg/foo/project/foo.install
  $ test ! -e _build/default/.lockfile/pkg/foo/project/omitted.install
  $ test ! -e _build/default/.lockfile/pkg/foo/.opam/foo/target

Enabling ordinary project targets does not grant writeback into their inputs.

  $ dune build --promote-install-files=true \
  >   _build/default/.lockfile/pkg/foo/project/foo.install
  $ test ! -e _source/project/foo.install
  $ test ! -e _source/project/omitted.install
