PPX driver metadata serializes replacement edges that are not ordinary library
requirements. A scoped package dependency includes the replaced driver.

  $ make_dune_project 3.24
  $ cat >>dune-project <<EOF
  > (package (name ppx-replacer))
  > (package (name ppx-original))
  > EOF

  $ mkdir replacer original consumer
  $ cat >replacer/dune <<'EOF'
  > (library
  >  (name replacer)
  >  (public_name ppx-replacer)
  >  (kind ppx_rewriter)
  >  (libraries compiler-libs.common)
  >  (ppx.driver
  >   (main "Replacer.main")
  >   (replaces ppx-original)))
  > EOF
  $ cat >replacer/replacer.ml <<'EOF'
  > let main () =
  >   let output = ref None in
  >   let input = ref None in
  >   for i = 1 to Array.length Sys.argv - 2 do
  >     match Sys.argv.(i) with
  >     | "-o" -> output := Some Sys.argv.(i + 1)
  >     | "--impl" -> input := Some Sys.argv.(i + 1)
  >     | _ -> ()
  >   done;
  >   match !input, !output with
  >   | Some input, Some output ->
  >     let ic = open_in_bin input in
  >     let oc = open_out_bin output in
  >     Fun.protect
  >       ~finally:(fun () -> close_in ic; close_out oc)
  >       (fun () -> output_string oc (really_input_string ic (in_channel_length ic)))
  >   | _ -> exit 2
  > EOF

  $ cat >original/dune <<'EOF'
  > (library
  >  (name original)
  >  (public_name ppx-original)
  >  (kind ppx_rewriter)
  >  (libraries compiler-libs.common)
  >  (modules ())
  >  (ppx.driver (main "(fun () -> Array.iter print_endline Sys.argv)")))
  > EOF

  $ cat >consumer/dune-project <<'EOF'
  > (lang dune 3.24)
  > EOF
  $ cat >consumer/dune <<'EOF'
  > (library
  >  (name user)
  >  (preprocess (pps ppx-replacer)))
  > EOF
  $ echo 'let value = ()' >consumer/user.ml

  $ cat >dune <<'EOF'
  > (rule
  >  (target result)
  >  (deps
  >   (package ppx-replacer)
  >   (source_tree consumer))
  >  (action
  >   (with-stdout-to %{target}
  >    (chdir consumer (run %{bin:dune} build user.cma)))))
  > EOF

  $ dune build result 2>err
  $ test ! -s err
