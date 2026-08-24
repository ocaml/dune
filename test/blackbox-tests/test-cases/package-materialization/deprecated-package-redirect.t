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

The scoped layout emits metadata for the intermediate deprecated package, so
findlib can follow the complete chain.

  $ dune build result

The generated dune-package metadata lets a nested Dune consumer follow the same
redirect chain.

  $ dune build dune-package-result && cat _build/default/dune-package-result
  42
