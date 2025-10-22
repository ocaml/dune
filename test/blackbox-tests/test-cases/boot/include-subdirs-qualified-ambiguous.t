Testing the bootstrap of a wrapped include subdirs qualified.
There is ambiguity between the two different foo modules.
We show that the behaviour is correct in the bootstrap process:
it picks up the closest one.

  $ . ./helpers.sh
  $ mkdir a

  $ cat > a/foo.ml << EOF
  > let msg = "shouldn't be printed"
  > EOF

  $ mkdir a/bar
  $ cat > a/bar/foo.ml << EOF
  > let msg = "the correct module"
  > EOF

  $ cat > a/bar/baz.ml << EOF
  > let exported = Foo.msg ^ "!"
  > EOF

  $ cat > a/dune <<EOF
  > (library
  >  (name a))
  > (include_subdirs qualified)
  > EOF

  $ create_dune a <<EOF
  > module M1 = A.Bar.Baz
  > let () = Printf.printf "Hello from %s\n" M1.exported
  > EOF
  ocamlc -output-complete-exe -intf-suffix .dummy -g -o .duneboot.exe -I boot -I +unix unix.cma boot/types.ml boot/libs.ml boot/duneboot.ml
  ./.duneboot.exe
  Hello from the correct module!
