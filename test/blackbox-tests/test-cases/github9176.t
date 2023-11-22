We check that `version = ""` in META files are correctly handled.

  $ export OPAM_SWITCH_PREFIX=`pwd`/opam-switch
  $ mkdir -p $OPAM_SWITCH_PREFIX/lib/mylib

  $ cat > $OPAM_SWITCH_PREFIX/lib/mylib/META << EOF
  > version=""
  > EOF

  $ cat > dune-project << EOF
  > (lang dune 1.0)
  > EOF

  $ cat > dune << EOF
  > (executable
  >  (name e)
  >  (libraries dune-build-info mylib))
  > EOF

  $ cat > e.ml << 'EOF'
  > let () =
  >   let open Build_info.V1 in
  >   let s =
  >     match Statically_linked_libraries.find ~name:"mylib" with
  >     | None -> "no lib"
  >     | Some l ->
  >       begin
  >         match Statically_linked_library.version l with
  >         | Some v when Version.to_string v = "" -> "empty string"
  >         | Some _ -> "other string"
  >         | None -> "no version"
  >       end
  >   in
  >   print_endline s
  > EOF

  $ dune build

In dune 3.11 and earlier, we would return "empty string" here.

  $ dune exec ./e.exe
  no version
