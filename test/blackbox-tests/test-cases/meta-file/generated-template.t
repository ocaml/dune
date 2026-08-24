Rule-generated META file templates are validated using their exact contents and
are diagnosed at their build path.

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
  File "_build/default/META.generated.template", line 1, characters 18-18:
  1 | package "broken" (
                        
  Error: Invalid META template for package generated.
  1 closing parentheses missing
  [1]
