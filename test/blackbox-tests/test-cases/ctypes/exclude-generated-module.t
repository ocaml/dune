Show error in ctypes when the `(modules ..)` stanza excludes the generated
module

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > (using ctypes 0.3)
  > (package (name a))
  > EOF

  $ cat > type_description.ml <<'EOF'
  > module Types (_ : Ctypes.TYPE) = struct
  > end
  > EOF

  $ cat > dune << EOF
  > (library
  >  (public_name a)
  >  (modules Type_description)
  >  (libraries ctypes.stubs)
  >  (ctypes
  >    (external_library_name libzstd)
  >    (type_description
  >     (instance Types)
  >     (functor Type_description))
  >    (generated_types Types_generated)
  >    (generated_entry_point C)))
  > EOF

  $ dune build 2>&1 | grep link_many -m 1 -A 2
    ("link_many: unable to find module",
     { main_module_name = "Libzstd__type_gen"
     ; modules =
  [1]
