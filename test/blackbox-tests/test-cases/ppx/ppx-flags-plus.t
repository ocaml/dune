Create ppx1 and exe:

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > EOF
  $ cat > dune <<EOF
  > (library
  >  (name ppx)
  >  (kind ppx_rewriter)
  >  (modules ppx)
  >  (ppx.driver (main Ppx.main)))
  > (executable
  >  (name the_exe)
  >  (modules the_exe)
  >  (preprocess (pps ppx --alert ++foo)))
  > EOF
  $ cat > ppx.ml <<EOF
  > let main () =
  >   let out = ref "" in
  >   let args =
  >     [ ("-o", Arg.Set_string out, "")
  >     ; ("--impl", Arg.Set_string (ref ""), "")
  >     ; ("--as-ppx", Arg.Set (ref false), "")
  >     ; ("--cookie", Arg.Set (ref false), "")
  >     ; ("--alert", Arg.Set_string (ref ""), "")
  >     ]
  >   in
  >   let anon _ = () in
  >   Arg.parse (Arg.align args) anon "";
  >   let out = open_out !out in
  >   close_out out;
  > EOF
  $ touch the_exe.ml

  $ dune build ./the_exe.exe
  File "dune", line 9, characters 30-35:
  9 |  (preprocess (pps ppx --alert ++foo)))
                                    ^^^^^
  Error: PPX args starting with `+' cannot be used before version 3.18 of the
  dune language
  Hint: Upgrade your dune-project to `(lang dune 3.18)'
  [1]

Works since Dune 3.18

  $ cat > dune-project <<EOF
  > (lang dune 3.18)
  > EOF
  $ dune build ./the_exe.exe
