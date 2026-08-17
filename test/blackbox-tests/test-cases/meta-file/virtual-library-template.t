Virtual-library incompatibility is diagnosed before running a generated META
file template action.

  $ make_dune_project_with_package 2.7 vlib

  $ cat >vlib.mli <<EOF
  > val foo : unit -> unit
  > EOF

  $ cat >dune <<'EOF'
  > (library
  >  (public_name vlib)
  >  (virtual_modules vlib))
  > 
  > (rule
  >  (target META.vlib.template)
  >  (action (run false)))
  > EOF

  $ dune build @install
  File "_build/default/META.vlib.template", line 1, characters 0-0:
  Error: Package vlib defines virtual library vlib and has a META template.
  This is not allowed.
  [1]
