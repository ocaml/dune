Test cases when melc is not available

  $ make_melange_project 3.8 0.1

  $ cat > main_melange.ml <<EOF
  > let () =
  >   print_endline "hello from melange"
  > EOF

Set up some fake environment without melc

  $ mkdir _path
  $ for bin in dune ocamlc ocamldep ocamlopt ocamlobjinfo; do
  >   if command -v "$bin" > /dev/null; then
  >     ln -s "$(command -v "$bin")" _path/
  >   fi
  > done

For melange.emit stanzas, an error is shown

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (emit_stdlib false)
  >  (modules main_melange)
  >  (alias mel))
  > EOF

  $ (unset INSIDE_DUNE; PATH=_path dune build --always-show-command-line --root . @mel 2>&1 | grep Program)
  Error: Program melc not found in the tree or in PATH
  [1]

For libraries, if no melange.emit stanza is found, build does not fail

  $ cat > dune <<EOF
  > (library
  >  (name lib1)
  >  (modules :standard \ main_native)
  >  (modes byte melange))
  > (executable
  >  (name main_native)
  >  (modules main_native)
  >  (modes exe byte)
  >  (libraries lib1))
  > EOF

  $ cat > main_native.ml <<EOF
  > let () =
  >   print_endline Lib1.Lib.t
  > EOF

  $ cat > lib.ml <<EOF
  > let t = "hello from native"
  > EOF

  $ (unset INSIDE_DUNE; PATH=_path dune build --display progress --always-show-command-line --root . main_native.bc)
  $ dune exec ./main_native.bc
  hello from native

If melange.emit stanza is found, but no rules are executed, build does not fail

  $ cat > dune <<EOF
  > (library
  >  (name lib1)
  >  (modules :standard \ main_native main_melange)
  >  (modes byte melange))
  > (executable
  >  (name main_native)
  >  (modules main_native)
  >  (modes exe byte)
  >  (libraries lib1))
  > (melange.emit
  >  (target output)
  >  (emit_stdlib false)
  >  (modules main_melange)
  >  (libraries lib1))
  > EOF

  $ (unset INSIDE_DUNE; PATH=_path dune build --display progress --always-show-command-line --root . main_native.bc)
  $ dune exec ./main_native.bc
  hello from native

But trying to build any melange artifacts will fail

  $ (unset INSIDE_DUNE; PATH=_path dune build --display progress  --always-show-command-line --root . output/main_melange.js)
  File "dune", lines 10-14, characters 0-94:
  10 | (melange.emit
  11 |  (target output)
  12 |  (emit_stdlib false)
  13 |  (modules main_melange)
  14 |  (libraries lib1))
  Error: Program melc not found in the tree or in PATH
   (context: default)
  Hint: opam install melange
  [1]

Mixed-mode libraries do not require melc for default aliases. This covers each
supported shape that combines OCaml modes with Melange.

  $ check_mixed_library () {
  >   dir="$1"
  >   modes="$2"
  >   mkdir "$dir"
  >   cat > "$dir/dune-project" <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF
  >   cat > "$dir/dune" <<EOF
  > (library
  >  (name mylib)
  >  (modules mylib)
  >  (modes $modes))
  > EOF
  >   cat > "$dir/mylib.ml" <<EOF
  > let t = "hello"
  > EOF
  >   (unset INSIDE_DUNE; PATH=$PWD/_path dune build --root "$dir" \
  >      @all @check > /dev/null 2>&1)
  >   cmj_dir="$dir/_build/default/.mylib.objs/melange"
  >   if test -d "$cmj_dir"; then
  >     cmj_count=$(find "$cmj_dir" -name '*.cmj' | wc -l)
  >     cmj_count=$(echo "$cmj_count" | tr -d ' ')
  >   else
  >     cmj_count=0
  >   fi
  >   echo "$dir: $cmj_count"
  > }

  $ check_mixed_library mixed-byte "byte melange"
  mixed-byte: 0
  $ check_mixed_library mixed-native "native melange"
  mixed-native: 0
  $ check_mixed_library mixed-byte-native "byte native melange"
  mixed-byte-native: 0
  $ check_mixed_library mixed-standard "melange :standard"
  mixed-standard: 0

Melange-only libraries still require melc

  $ mkdir melange-only
  $ cat > melange-only/dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF
  $ cat > melange-only/dune <<EOF
  > (library
  >  (name mylib)
  >  (modules mylib)
  >  (modes melange))
  > EOF
  $ cat > melange-only/mylib.ml <<EOF
  > let t = "hello"
  > EOF

  $ missing_melc_for_alias () {
  >   alias="$1"
  >   (unset INSIDE_DUNE; PATH=$PWD/_path dune build \
  >      --root melange-only --always-show-command-line "$alias" 2>&1 \
  >      | grep Program)
  > }

  $ missing_melc_for_alias @all
  Error: Program melc not found in the tree or in PATH
  [1]
  $ missing_melc_for_alias @check
  Error: Program melc not found in the tree or in PATH
  [1]
