The same authored project uses the same directory rules as a workspace project
and as a mounted dependency. Include-subdirs groups retain their source view.

  $ make_lockdir
  $ make_named_package_project main 3.25 foo
  $ mkdir -p _source/group/sub
  $ (cd _source && make_dune_project_with_package 3.25 foo)
  $ cat > _source/dune <<'EOF'
  > (library (public_name foo) (modules foo))
  > (executable
  >  (name verify)
  >  (modules verify)
  >  (libraries foo foo.group))
  > (rule
  >  (target verified)
  >  (deps foo.cma group/group.cma)
  >  (action (with-stdout-to %{target} (run ./verify.exe))))
  > EOF
  $ echo 'let value = 1' > _source/foo.ml
  $ cat > _source/verify.ml <<'EOF'
  > let () =
  >   if Foo.value <> 1 || Group.Sub.Value.value <> 2 then
  >     failwith "incorrect include-subdirs values";
  >   print_endline "verified"
  > EOF
  $ cat > _source/group/dune <<'EOF'
  > (include_subdirs qualified)
  > (library (name group) (public_name foo.group))
  > EOF
  $ echo 'let value = 2' > _source/group/sub/value.ml

Run the project normally first.

  $ (cd _source && dune build --root . --only-packages foo verified)
  $ cat _source/_build/default/verified
  verified

Mount the identical source. The lock recipe is deliberately unusable, so the
same rules must be generated in the current graph. Workspace package selection
must not replace the mounted project's configuration.

  $ make_dune_source_lockpkg native foo "$PWD/_source"
  $ dune build --only-packages main _build/default/.pkg/foo/verified
  $ cat _build/default/.pkg/foo/verified
  verified

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
  $ make_dune_source_lockpkg native bar "$PWD/_source"

  $ dune build _build/default/.pkg/bar/nested/bar.cma
  $ find _build/default/.pkg/bar/.ppx -name ppx.exe | wc -l
  1
  $ test ! -e _build/default/.pkg/bar/nested/.ppx

With distinct host and target contexts, both builds put the driver in the host
partition while compiling the consuming library in the target context.

  $ (cd _source && make_host_target_workspace)
  $ (cd _source && dune build --root . --only-packages bar _build/target/nested/bar.cma)
  $ find _source/_build/host/.ppx -name ppx.exe | wc -l
  1
  $ test ! -e _source/_build/target/.ppx
  $ make_host_target_workspace
  $ dune build _build/target/.pkg/bar/nested/bar.cma
  $ find _build/host/.pkg/bar/.ppx -name ppx.exe | wc -l
  1
  $ test ! -e _build/target/.pkg/bar/.ppx
  $ rm dune-workspace _source/dune-workspace

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
  $ (cd _source && dune build --root . --only-packages foo _build/default/traversed)
  scanned

  $ dune build _build/default/.pkg/foo/traversed
  scanned
  $ cat _build/default/.pkg/foo/traversed
  traversed

Implicit default aliases use the same source view and include ordinary targets.

  $ cat >> _source/checks/child/dune <<'EOF'
  > (rule (target implicit) (action (write-file %{target} implicit)))
  > EOF
  $ (cd _source && dune build --root . --only-packages foo @_build/default/checks/child/default)
  $ cat _source/_build/default/checks/child/implicit
  implicit
  $ dune build @_build/default/.pkg/foo/checks/child/default
  $ cat _build/default/.pkg/foo/checks/child/implicit
  implicit

OCaml test entry points continue to resolve through the same directory contents.

  $ mkdir _source/unit
  $ cat > _source/unit/dune <<'EOF'
  > (test (name smoke) (libraries foo))
  > EOF
  $ echo 'let () = print_endline (if Foo.value = 1 then "unit" else "bad")' > _source/unit/smoke.ml
  $ (cd _source && dune runtest --root . _build/default/unit/smoke.ml)
  unit
  $ dune runtest _build/default/.pkg/foo/unit/smoke.ml
  unit

Copy-files globs include generated files as well as authored sources, including
when the input directory is a descendant of the copying directory. The
workspace behaviour is covered by stanzas/copy_files/test6.t.

  $ mkdir -p _source/copies/input
  $ echo '(copy_files input/*.txt)' > _source/copies/dune
  $ echo authored > _source/copies/input/source.txt
  $ cat > _source/copies/input/dune <<'EOF'
  > (rule (target generated.txt) (action (write-file %{target} generated)))
  > EOF
  $ dune build _build/default/.pkg/foo/copies/source.txt
  $ cat _build/default/.pkg/foo/copies/source.txt
  authored

  $ dune build _build/default/.pkg/foo/copies/generated.txt
  $ cat _build/default/.pkg/foo/copies/generated.txt
  generated

Copy-with-line-directive actions and globs retain workspace source locations,
including when sandboxed. Mounted copies name the original file through its
mount, without leaking a sandbox path.

  $ mkdir -p _source/line-directives/input
  $ echo 'let value = 42' > _source/line-directives/input/value.ml
  $ echo 'int value = 42;' > _source/line-directives/input/value.c
  $ cat > _source/line-directives/dune <<'EOF'
  > (rule
  >  (target copied.ml)
  >  (deps (sandbox always))
  >  (action (copy# input/value.ml %{target})))
  > (rule
  >  (target copied.c)
  >  (deps (sandbox always))
  >  (action (copy# input/value.c %{target})))
  > (copy_files# input/*.ml)
  > (copy_files# (only_sources) (files input/*.c))
  > EOF
  $ (cd _source && dune build --root . --only-packages foo --sandbox copy \
  >   line-directives/copied.ml line-directives/value.ml \
  >   line-directives/copied.c line-directives/value.c)
  $ for file in copied.ml value.ml copied.c value.c; do
  >   cat _source/_build/default/line-directives/$file
  > done
  # 1 "line-directives/input/value.ml"
  let value = 42
  # 1 "line-directives/input/value.ml"
  let value = 42
  #line 1 "line-directives/input/value.c"
  int value = 42;
  #line 1 "line-directives/input/value.c"
  int value = 42;
  $ dune build $pkg_root/foo/line-directives/{copied,value}.{ml,c}
  $ for file in copied.ml value.ml copied.c value.c; do
  >   cat _build/default/.pkg/foo/line-directives/$file
  > done
  # 1 "_build/default/.pkg/foo/line-directives/input/value.ml"
  let value = 42
  # 1 "_build/default/.pkg/foo/line-directives/input/value.ml"
  let value = 42
  #line 1 "_build/default/.pkg/foo/line-directives/input/value.c"
  int value = 42;
  #line 1 "_build/default/.pkg/foo/line-directives/input/value.c"
  int value = 42;

Only-sources globs exclude generated inputs and remove stale copied targets.
Adding a source file updates the glob.

  $ echo '(copy_files (only_sources) (files input/*.txt))' > _source/copies/dune
  $ dune build _build/default/.pkg/foo/copies/source.txt
  $ test ! -e _build/default/.pkg/foo/copies/generated.txt
  $ echo added > _source/copies/input/added.txt
  $ dune build _build/default/.pkg/foo/copies/added.txt
  $ cat _build/default/.pkg/foo/copies/added.txt
  added

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
  $ dune build _build/default/.pkg/foo/dir-consumer/generated.txt
  $ cat _build/default/.pkg/foo/dir-consumer/generated.txt
  directory

An only-sources glob does not see the files of a directory target, in either
view.

  $ echo '(copy_files (only_sources) (files ../dir-producer/files/*.txt))' > _source/dir-consumer/dune
  $ (cd _source && dune build --root . --only-packages foo _build/default/dir-consumer/generated.txt 2>&1 | grep Error)
  Error: Does not match any files
  [1]
  $ dune build _build/default/.pkg/foo/dir-consumer/generated.txt 2>&1 | grep Error
  Error: Does not match any files
  [1]
  $ echo '(copy_files ../dir-producer/files/*.txt)' > _source/dir-consumer/dune

Promoted directory targets can replace authored directories in the build tree.
A mounted build must retain the source mask without promoting.

  $ mkdir -p _source/promoted/dir
  $ echo authored > _source/promoted/dir/source.txt
  $ cat > _source/promoted/dune <<'EOF'
  > (rule
  >  (mode promote)
  >  (target (dir dir))
  >  (deps (sandbox always))
  >  (action (system "mkdir dir; echo promoted > dir/generated.txt")))
  > EOF
  $ dune build _build/default/.pkg/foo/promoted/dir/generated.txt
  $ cat _build/default/.pkg/foo/promoted/dir/generated.txt
  promoted
  $ test ! -e .pkg
  $ (cd _source && dune build --root . --only-packages foo _build/default/promoted/dir/generated.txt)
  $ cat _source/_build/default/promoted/dir/generated.txt
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
  $ make_named_package_project main 3.25 dep
  $ mkdir -p _source/warning _source/modules
  $ (cd _source && make_dune_project_with_package 3.25 dep)
  $ cat > _source/warning/dune <<'EOF'
  > (library (name warning) (public_name dep.warning))
  > EOF
  $ echo 'let value = let unused = 0 in 42' > _source/warning/warning.ml
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
  $ (cd _source && dune build --root . modules/modules.cma) 2>workspace-modules
  [1]
  $ grep '^Error' workspace-modules
  Error: These modules appear in the private_modules field:

Vendoring suppresses warnings and tolerates private modules omitted from the
modules field.

  $ (cd _vendor && dune build --root . dep/warning/warning.cma dep/modules/modules.cma)

The mounted project receives the same treatment, with no warnings.

  $ make_lockdir
  $ make_dune_source_lockpkg native dep "$PWD/_source"
  $ mounted=$pkg_root/dep
  $ dune build "$mounted/warning/warning.cma"
  $ dune build "$mounted/modules/modules.cma"

The dependency warning context also applies while decoding stanzas, not only
when compiling OCaml. no_keep_locs is a warning whose producer raises an error
in recent language versions; authored vendoring suppresses it.

  $ mkdir _source/syntax _vendor/dep/syntax
  $ cat > _source/syntax/dune <<'EOF'
  > (library (name syntax) (public_name dep.syntax) (no_keep_locs))
  > EOF
  $ echo 'let value = 42' > _source/syntax/syntax.ml
  $ cp _source/syntax/dune _source/syntax/syntax.ml _vendor/dep/syntax/
  $ (cd _source && dune build --root . syntax/syntax.cma) 2>workspace-syntax
  [1]
  $ grep '^no_keep_locs' workspace-syntax
  no_keep_locs is a no-op. Please delete it.
  $ (cd _vendor && dune build --root . dep/syntax/syntax.cma)
  $ dune build "$mounted/syntax/syntax.cma"

Warnings emitted by generated project rules must use that same context.

  $ echo '(library (name syntax) (public_name dep.syntax))' > _source/syntax/dune
  $ cp _source/syntax/dune _vendor/dep/syntax/dune
  $ cat > _source/dune-project <<'EOF'
  > (lang dune 3.25)
  > (generate_opam_files true)
  > (package (name dep) (depends helper helper))
  > EOF
  $ cp _source/dune-project _vendor/dep/dune-project
  $ cat > _source/dune <<'EOF'
  > (rule (target probe) (action (write-file %{target} probe)))
  > EOF
  $ cp _source/dune _vendor/dep/dune
  $ (cd _source && dune build --root . probe) 2>workspace-project
  $ grep -c 'Duplicate dependency' workspace-project
  1
  $ (cd _vendor && dune build --root . dep/probe) 2>vendor-project
  $ grep -c 'Duplicate dependency' vendor-project
  0
  [1]
  $ dune build "$mounted/probe" 2>mounted-project
  $ grep -c 'Duplicate dependency' mounted-project
  0
  [1]

Recursive globs can reach a sibling directory, even above the current project
root. Rule dependencies and install entries must traverse the same input tree.

  $ mkdir ../relative-globs
  $ cd ../relative-globs
  $ make_dune_project 3.25
  $ make_lockdir
  $ mkdir -p _source/project/sub _source/data/nested
  $ echo '(lang dune 3.25)' > _source/dune-project
  $ (cd _source/project && make_dune_project_with_package 3.25 foo)
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
  $ make_dune_source_lockpkg native foo "$PWD/_source"
  $ cat > dune <<'EOF'
  > (rule
  >  (target installed)
  >  (action (copy %{pkg:foo:share:data/nested/second.txt} %{target})))
  > EOF
  $ dune build _build/default/.pkg/foo/project/sub/result installed
  $ cat _build/default/.pkg/foo/project/sub/result
  first
  second
  $ cat _build/default/installed
  second

Changes to sibling inputs invalidate both uses of the glob.

  $ echo changed > _source/data/nested/second.txt
  $ dune build _build/default/.pkg/foo/project/sub/result installed
  $ cat _build/default/.pkg/foo/project/sub/result
  first
  changed
  $ cat _build/default/installed
  changed

Project-local installation targets and aliases are ordinary rules too.

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

The mounted aliases build the same project targets without running the recipe.
A locked package is not installed in the workspace install directory, so it
has no install file.

  $ dune build @_build/default/.pkg/foo/project/ocaml-index
  $ test -f _build/default/.pkg/foo/project/.foo.objs/cctx.ocaml-index
  $ dune build _build/default/.pkg/foo/project/foo.install
  Error: Don't know how to build _build/default/.pkg/foo/project/foo.install
  [1]

An error about a mounted directory names it by its build path.

  $ mkdir _dup
  $ (cd _dup && make_dune_project_with_package 3.25 dup)
  $ echo '(library (public_name dup))' > _dup/dune
  $ touch _dup/x.ml _dup/X.ml
  $ make_dune_source_lockpkg native dup "$PWD/_dup"
  $ dune build _build/default/.pkg/dup/dup.cma 2>&1 | grep "Too many"
  Error: Too many files for module X in _build/default/.pkg/dup:
  [1]

A copy_files glob cannot leave the mounted package.

  $ mkdir -p _esc/sub ws
  $ echo secret > ws/secret.txt
  $ (cd _esc && make_dune_project_with_package 3.25 esc)
  $ echo '(copy_files ../../../ws/*.txt)' > _esc/sub/dune
  $ make_dune_source_lockpkg native esc "$PWD/_esc"
  $ dune build _build/default/.pkg/esc/sub/secret.txt 2>&1 | head -8
  File "_build/default/.pkg/esc/sub/dune", line 1, characters 12-29:
  Error: ../../../ws/*.txt is outside the package esc. A locked package can
  only copy its own files.
  [1]

An error from the project model names a mounted directory by its build path.

  $ mkdir -p _excl/sub
  $ cat > _excl/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name excl) (dir sub))
  > EOF
  $ touch _excl/x
  $ echo '(install (package excl) (section share) (files x))' > _excl/dune
  $ make_dune_source_lockpkg native excl "$PWD/_excl"
  $ dune build @_build/default/.pkg/excl/all 2>&1 | grep "must exist"
  That package must exist in _build/default/.pkg/excl/sub
  [1]
