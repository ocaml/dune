The empty list and atoms are printed as is:

  $ echo '()' | dune format-dune-file
  ()

  $ echo 'a' | dune format-dune-file
  a

Lists containing only atoms, quoted strings, templates, and singleton lists are
printed inline:

  $ echo '(atom "string" %{template} (singleton))' | dune format-dune-file
  (atom "string" %{template} (singleton))

Other lists are displayed one element per line:

  $ echo '(a (b c d) e)' | dune format-dune-file
  (a
   (b c d)
   e)

When there are several s-expressions, they are printed with an empty line
between them:

  $ echo '(a b) (c d)' | dune format-dune-file
  (a b)
  
  (c d)

It is possible to pass a file name:

  $ dune format-dune-file dune
  (a b)

Parse errors are displayed:

  $ echo '(' | dune format-dune-file
  File "", line 2, characters 0-0:
  Error: unclosed parenthesis at end of input
  [1]

When a list is indented, there is no extra space at the end.

  $ echo ' (a (b (c d)))' | dune format-dune-file
  (a
   (b
    (c d)))

When there is a long list of atoms, quoted strings, templates and singletons,
it gets wrapped.

  $ echo '(library (name dune) (libraries unix stdune fiber xdg dune_re threads opam_file_format dune_lang ocaml_config which_program) (synopsis "Internal Dune library, do not use!") (preprocess  (action (run %{project_root}/src/let-syntax/pp.exe %{input-file}))))' | dune format-dune-file
  (library
   (name dune)
   (libraries
    unix
    stdune
    fiber
    xdg
    dune_re
    threads
    opam_file_format
    dune_lang
    ocaml_config
    which_program)
   (synopsis "Internal Dune library, do not use!")
   (preprocess
    (action
     (run %{project_root}/src/let-syntax/pp.exe %{input-file}))))

In multi-line strings, newlines are escaped, but their syntax is not preserved.

  $ dune format-dune-file <<EOF
  > (echo "\> multi
  >       "\> line
  >       "\> string
  >       "\| string
  > )
  > 
  > (echo "\
  > multi
  > line
  > string
  > ")
  > EOF
  (echo "multi\nline\nstring\nstring\n")
  
  (echo "multi\nline\nstring\n")

Comments are preserved.

  $ dune format-dune-file <<EOF
  > ; comment
  > (; first comment
  > a b; comment for b
  > ccc; multi
  > ; line
  > ; comment for ccc
  > d
  > e
  > ; unattached comment
  > f
  > ; unattached
  > ; multi-line
  > ; comment
  > g
  > )
  > EOF
  ; comment
  
  (; first comment
   a
   b ; comment for b
   ccc ; multi
       ; line
       ; comment for ccc
   d
   e
   ; unattached comment
   f
   ; unattached
   ; multi-line
   ; comment
   g)

When a comment is at the end of a list, the ")" is on a own line.

  $ dune format-dune-file <<EOF
  > (a ; final attached comment
  > )
  > (a ; final multiline
  > ; comment
  >  )
  > (a
  >  ; final unattached
  >  )
  > (a
  >  ; final unattached
  >  ; multiline
  >  )
  > EOF
  (a ; final attached comment
   )
  
  (a ; final multiline
     ; comment
   )
  
  (a
   ; final unattached
   )
  
  (a
   ; final unattached
   ; multiline
   )

Files in OCaml syntax are copied verbatim (but error when passed in stdin).

  $ dune format-dune-file < ocaml-syntax.dune
  File "", line 1, characters 0-20:
  Error: OCaml syntax is not supported.
  [1]
  $ dune format-dune-file ocaml-syntax.dune
  (* -*- tuareg -*- *)
  
  let () = Jbuild_plugin.V1.send {|
  (alias
   (name runtest)
   (action (echo "ocaml syntax")))
  |}

Non 0 error code:

  $ echo "(" | dune format-dune-file ; echo $?
  File "", line 2, characters 0-0:
  Error: unclosed parenthesis at end of input
  1
