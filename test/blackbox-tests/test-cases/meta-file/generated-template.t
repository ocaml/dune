Rule-generated META file templates are currently accepted when malformed. The
rule omits a final newline so the diagnostic must use the exact contents.

  $ make_dune_project_with_package 2.7 generated

  $ cat >generated.ml <<EOF
  > let foo () = ()
  > EOF

  $ cat >dune <<'EOF'
  > (library
  >  (public_name generated))
  > 
  > (rule
  >  (target META.generated.template)
  >  (action
  >   (write-file %{target} "package \"broken\" (")))
  > EOF

  $ dune build @install

The generated template has the exact unterminated contents, including no final
newline, and those contents reach the generated META file.

  $ printf 'package "broken" (' | cmp - _build/default/META.generated.template
  $ grep '^package "broken" (' _build/default/META.generated
  package "broken" (
