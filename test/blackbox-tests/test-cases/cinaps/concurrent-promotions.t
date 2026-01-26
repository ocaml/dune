Cinaps should offer all promotions at once

  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (using cinaps 1.3)
  > EOF

  $ cat > t1.ml <<"EOF"
  > (*$ print_endline "\nhello" *)
  > (*$*)
  > let x = 1
  > EOF

  $ cat > t2.ml <<"EOF"
  > (*$ print_endline "\nhello" *)
  > (*$*)
  > let x = 1
  > EOF

  $ cat >dune <<EOF
  > (cinaps
  >  (files *.ml)
  >  (alias cinaps))
  > EOF

  $ dune build @cinaps
  File "t1.ml", line 1, characters 0-0:
  --- t1.ml
  +++ t1.ml.cinaps-corrected
  @@ -1,3 +1,4 @@
   (*$ print_endline "\nhello" *)
  +hello
   (*$*)
   let x = 1
  File "t2.ml", line 1, characters 0-0:
  --- t2.ml
  +++ t2.ml.cinaps-corrected
  @@ -1,3 +1,4 @@
   (*$ print_endline "\nhello" *)
  +hello
   (*$*)
   let x = 1
  [1]
