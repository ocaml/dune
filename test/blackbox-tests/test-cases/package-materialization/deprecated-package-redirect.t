A selected redirect can belong to a deprecated package name. Its owning package
must then contribute the separate META and dune-package files generated under
that deprecated name.

  $ make_dune_project 3.24
  $ cat >>dune-project <<'EOF'
  > (package (name redirect-root))
  > (package
  >  (name redirect-owner)
  >  (deprecated_package_names old-support))
  > (package (name redirect-target))
  > EOF

  $ mkdir root owner target consumer
  $ cat >root/dune <<'EOF'
  > (deprecated_library_name
  >  (old_public_name redirect-root.old)
  >  (new_public_name old-support.lib))
  > EOF

  $ cat >owner/dune <<'EOF'
  > (deprecated_library_name
  >  (old_public_name old-support.lib)
  >  (new_public_name redirect-target))
  > EOF

  $ cat >target/dune <<'EOF'
  > (library
  >  (name redirect_target)
  >  (public_name redirect-target))
  > EOF
  $ echo 'let value = 42' >target/redirect_target.ml

  $ cat >consumer/dune-project <<'EOF'
  > (lang dune 3.24)
  > EOF
  $ cat >consumer/dune <<'EOF'
  > (executable
  >  (name main)
  >  (libraries redirect-root.old))
  > EOF
  $ echo 'let () = print_int Redirect_target.value' >consumer/main.ml

  $ cat >dune <<'EOF'
  > (rule
  >  (target result)
  >  (deps (package redirect-root))
  >  (action
  >   (with-stdout-to %{target}
  >    (run %{bin:ocamlfind} query -recursive redirect-root.old))))
  > (rule
  >  (target dune-package-result)
  >  (deps
  >   (package redirect-root)
  >   (source_tree consumer))
  >  (action
  >   (with-stdout-to %{target}
  >    (chdir consumer (run %{bin:dune} exec ./main.exe)))))
  > EOF

The ordinary package metadata can name the intermediate redirect, but the
scoped layout currently has no metadata directory for its deprecated package.

  $ dune build result
  File "dune", lines 1-6, characters 0-155:
  1 | (rule
  2 |  (target result)
  3 |  (deps (package redirect-root))
  4 |  (action
  5 |   (with-stdout-to %{target}
  6 |    (run %{bin:ocamlfind} query -recursive redirect-root.old))))
  ocamlfind: Package `old-support.lib' not found - required by `redirect-root.old'
  [1]

The nested Dune consumer likewise cannot resolve the intermediate package.

  $ dune build dune-package-result
  File "dune", line 3, characters 12-29:
  3 |  (libraries redirect-root.old))
                  ^^^^^^^^^^^^^^^^^
  Error: Library "redirect-root.old" not found.
  -> required by _build/default/.main.eobjs/native/dune__exe__Main.cmx
  -> required by _build/default/main.exe
  [1]
