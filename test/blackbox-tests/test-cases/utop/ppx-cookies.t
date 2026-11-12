Custom toplevels and UTop pass string-valued cookies to their runtime PPX
commands.

The driver reports cookie arguments only for runtime invocations and copies
the compiler's input unchanged. Building the toplevel itself stays quiet.
Debug information is unrelated to cookie passing and slows fixture linking.

  $ export OCAMLPARAM="${OCAMLPARAM:-_},g=0"
  $ make_dune_project 3.25
  $ cat >dune <<'EOF'
  > (library
  >  (name cookie_ppx)
  >  (modules cookie_ppx)
  >  (kind (ppx_rewriter (cookies (message "hello"))))
  >  (ppx.driver
  >   (main Cookie_ppx.main)
  >   (flags ())))
  > (toplevel
  >  (name tt)
  >  (preprocess (pps cookie_ppx)))
  > EOF
  $ cat >cookie_ppx.ml <<'EOF'
  > let main () =
  >   let runtime = ref false in
  >   let cookies = ref [] in
  >   let input_file = ref "" in
  >   let output_file = ref "" in
  >   let positional = ref [] in
  >   Arg.parse
  >     [ "--as-ppx", Arg.Set runtime, ""
  >     ; "--cookie", Arg.String (fun s -> cookies := s :: !cookies), ""
  >     ; "--impl", Arg.Set_string input_file, ""
  >     ; "-o", Arg.Set_string output_file, ""
  >     ]
  >     (fun s -> positional := s :: !positional) "";
  >   (match List.rev !positional with
  >    | [] -> ()
  >    | [src; dst] -> input_file := src; output_file := dst
  >    | _ -> failwith "unexpected PPX arguments");
  >   if !runtime then
  >     List.iter print_endline (List.sort String.compare !cookies);
  >   let ic = open_in_bin !input_file in
  >   let contents = really_input_string ic (in_channel_length ic) in
  >   close_in ic;
  >   let oc = open_out_bin !output_file in
  >   output_string oc contents;
  >   close_out oc
  > EOF
  $ echo '()' >init.ml

Both toplevels preserve the cookie's string value. Dune does not supply the
compilation-mode cookie before language version 3.26.

  $ dune exec ./tt.exe -- init.ml
  message="hello"
  $ dune utop . -- init.ml
  message="hello"

Changing the language version supplies the cookie to both runtime PPX commands,
without declaring it in the PPX library.

  $ make_dune_project 3.26
  $ dune exec ./tt.exe -- init.ml
  compilation-mode="ocaml"
  message="hello"
  $ dune utop . -- init.ml
  compilation-mode="ocaml"
  message="hello"
