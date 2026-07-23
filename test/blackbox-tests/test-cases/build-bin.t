Pform expansion should work in `exec` but also in `build`:

  $ make_dune_project_with_package 3.10 public
  $ cat > dune << EOF
  > (executable (public_name public) (modules Public))
  > (executable (name private) (modules Private))
  > (env
  >   (_ (binaries (private.exe as priv))))
  > EOF

The executables in question just display what they are:

  $ cat > public.ml << EOF
  > let () = print_endline "Public"
  > EOF
  $ cat > private.ml << EOF
  > let () = print_endline "Private"
  > EOF

We also have a subfolder `foo`

  $ mkdir foo
  $ cat > foo/dune << EOF
  > (executable (public_name blah))
  > (env
  >   (_ (binaries (blah.exe as foo))))
  > EOF
  $ cat > foo/blah.ml << EOF
  > let () = print_endline "Blah"
  > EOF

Making Dune build the executables should work without error messages:

  $ dune build %{bin:public}
  $ ls _build/default/public.exe
  _build/default/public.exe

Building the private executable should also work.

  $ dune build %{bin:private}
  File "command line", line 1, characters 0-14:
  Error: Program private not found in the tree or in PATH
   (context: default)
  [1]
  $ ls _build/default/private.exe > /dev/null 2>&1 || return 1
  [1]

Building the private executable via the env alias should work too:

  $ dune build %{bin:priv}

  $ (cd foo && dune build --root .. %{bin:priv}) # should work
  $ dune build %{bin:foo} # doesn't work because foo is underneath us
  File "command line", line 1, characters 0-10:
  Error: Program foo not found in the tree or in PATH
   (context: default)
  [1]
  $ (cd foo && dune build --root .. %{bin:foo}) # should work
  Entering directory '..'
  File "command line", line 1, characters 0-10:
  Error: Program foo not found in the tree or in PATH
   (context: default)
  Leaving directory '..'
  [1]
