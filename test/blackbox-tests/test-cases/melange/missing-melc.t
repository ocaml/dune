Test cases when melc is not available

  $ cat > dune-project <<EOF
  > (lang dune 3.6)
  > (using melange 0.1)
  > EOF

  $ cat > main.ml <<EOF
  > let () =
  >   print_endline "hello"
  > EOF

For melange.emit stanzas

  $ cat > dune <<EOF
  > (melange.emit
  >  (target output)
  >  (module_system commonjs))
  > EOF

  $ melc_path=$(which melc)
  $ melc_path_tmp="$melc_path.tmp"
  $ mv $melc_path $melc_path_tmp
  $ dune build output/melange__Main.js
  Error: A melange.emit stanza was found, but the melange compiler melc is not
  available. Either install it by running [opam install melange], or remove the
  melange.emit stanzas from the project.
  [1]
  $ mv $melc_path_tmp $melc_path

For libraries, if no melange.emit stanza is found, build does not fail

  $ cat > dune <<EOF
  > (library
  >  (name lib1)
  >  (modules :standard \ main)
  >  (modes byte native melange))
  > (executable
  >  (name main)
  >  (modules main)
  >  (modes exe byte)
  >  (libraries lib1))
  > EOF

  $ cat > main.ml <<EOF
  > let () =
  >   print_endline Lib1.Lib.t
  > EOF

  $ cat > lib.ml <<EOF
  > let t = "hello"
  > EOF

  $ melc_path=$(which melc)
  $ melc_path_tmp="$melc_path.tmp"
  $ mv $melc_path $melc_path_tmp
  $ dune exec ./main.exe
  hello
  $ mv $melc_path_tmp $melc_path

But if melange.emit stanza is found, build will fail eagerly

  $ cat > dune <<EOF
  > (library
  >  (name lib1)
  >  (modules :standard \ main main_melange)
  >  (modes byte native melange))
  > (executable
  >  (name main)
  >  (modules main)
  >  (modes exe byte)
  >  (libraries lib1))
  > (melange.emit
  >  (target output)
  >  (entries main_melange)
  >  (libraries lib1)
  >  (module_system commonjs))
  > EOF

  $ cat > main_melange.ml <<EOF
  > let () =
  >   print_endline Lib1.Lib.t
  > EOF

  $ melc_path=$(which melc)
  $ melc_path_tmp="$melc_path.tmp"
  $ mv $melc_path $melc_path_tmp
  $ dune exec ./main.exe
  Error: A melange.emit stanza was found, but the melange compiler melc is not
  available. Either install it by running [opam install melange], or remove the
  melange.emit stanzas from the project.
  [1]
  $ mv $melc_path_tmp $melc_path
