A ppx driver of a nested project can link dune-build-info.

  $ make_dune_project 3.25
  $ mkdir -p sub/driver
  $ (cd sub && make_dune_project 3.25)
  $ cat > sub/dune <<'EOF'
  > (library (name bar) (preprocess (staged_pps identity)))
  > EOF
  $ echo 'let value = 3' > sub/bar.ml
  $ cat > sub/driver/dune <<'EOF'
  > (library
  >  (name identity)
  >  (kind ppx_rewriter)
  >  (libraries dune-build-info)
  >  (ppx.driver (main Identity.main)))
  > EOF
  $ cat > sub/driver/identity.ml <<'EOF'
  > let main () =
  >   ignore (Build_info.V1.version ());
  >   let input = open_in_bin Sys.argv.(Array.length Sys.argv - 2) in
  >   let contents = really_input_string input (in_channel_length input) in
  >   close_in input;
  >   let output = open_out_bin Sys.argv.(Array.length Sys.argv - 1) in
  >   output_string output contents;
  >   close_out output
  > EOF
  $ dune build sub/bar.cma
