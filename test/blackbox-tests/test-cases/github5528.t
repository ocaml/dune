  $ cat > dune-project <<EOF
  > (lang dune 1.0)
  > EOF

  $ cat > dune <<EOF
  > (test
  >   (name t))
  > EOF

  $ cat > t.ml <<EOF
  > type color =  Normal | Cyan
  > 
  > let int_of_color = function
  >   | Normal -> 0
  >   | Cyan   -> 6
  > 
  > let in_color c pp out x =
  >   let n = int_of_color c in
  >   Printf.fprintf out "\x1b[3%dm" n;
  >   pp out x;
  >   Printf.fprintf out "\x1b[0m"
  > 
  > let reset_line = "\x1b[2K\r"
  > 
  > let () =
  >   Printf.printf "%sVery Secret!\n%!" reset_line;
  >   Printf.printf "%s\n%!" (String.make 15 '-');
  >   Printf.printf "%a\n%!" (in_color Cyan output_string) "Can you see it?"
  > EOF

  $ dune runtest -f
  Very Secret!
  ---------------
  Can you see it?

  $ dune exec ./t.exe
  Can you see it?
