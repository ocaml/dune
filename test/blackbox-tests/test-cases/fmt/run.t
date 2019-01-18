The empty list and atoms are printed as is:

  $ echo '()' | dune unstable-fmt
  ()

  $ echo 'a' | dune unstable-fmt
  a

Lists containing only atoms, quoted strings, templates, and singleton lists are
printed inline:

  $ echo '(atom "string" %{template} (singleton))' | dune unstable-fmt
  (atom "string" %{template} (singleton))

Other lists are displayed one element per line:

  $ echo '(a (b c d) e)' | dune unstable-fmt
  (a
   (b c d)
   e)

When there are several s-expressions, they are printed with an empty line
between them:

  $ echo '(a b) (c d)' | dune unstable-fmt
  (a b)
  
  (c d)

It is possible to pass a file name:

  $ dune unstable-fmt dune
  (a b)

A file can be fixed in place:

  $ echo '(a (b c))' > dune_temp
  $ dune unstable-fmt --inplace dune_temp
  $ cat dune_temp
  (a
   (b c))

The --inplace flag requires a file name:

  $ dune unstable-fmt --inplace
  --inplace requires a file name
  [1]

Parse errors are displayed:

  $ echo '(' | dune unstable-fmt
  Parse error: unclosed parenthesis at end of input

and files are not removed when there is an error:

  $ echo '(a' > dune_temp
  $ dune unstable-fmt --inplace dune_temp
  Parse error: unclosed parenthesis at end of input
  $ cat dune_temp
  (a

When a list is indented, there is no extra space at the end.

  $ echo ' (a (b (c d)))' | dune unstable-fmt
  (a
   (b
    (c d)))

When there is a long list of atoms, quoted strings, templates and singletons,
it gets wrapped.

  $ echo '(library (name dune) (libraries unix stdune fiber xdg dune_re threads opam_file_format dune_lang ocaml_config which_program) (synopsis "Internal Dune library, do not use!") (preprocess  (action (run %{project_root}/src/let-syntax/pp.exe %{input-file}))))' | dune unstable-fmt
  (library
   (name dune)
   (libraries unix stdune fiber xdg dune_re threads opam_file_format dune_lang
     ocaml_config which_program)
   (synopsis "Internal Dune library, do not use!")
   (preprocess
    (action
     (run %{project_root}/src/let-syntax/pp.exe %{input-file}))))

In multi-line strings, newlines are escaped.

  $ dune unstable-fmt <<EOF
  > (echo "\> multi
  >       "\> line
  >       "\> string
  > )
  > 
  > (echo "\
  > multi
  > line
  > string
  > ")
  > EOF
  (echo "multi\nline\nstring\n")
  
  (echo "multi\nline\nstring\n")

Comments are preserved.

  $ dune unstable-fmt <<EOF
  > ; comment
  > (;first comment
  > a b;comment for b
  > ccc;multi
  > ;line
  > ;comment for ccc
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
  ;  comment
  
  (; first comment
   a
   b ; comment for b
   ccc ; multi
       ; line
       ; comment for ccc
   d
   e
   ;  unattached comment
   f
   ;  unattached
   ;  multi-line
   ;  comment
   g)

When a comment is at the end of a list, the ")" is on a own line.

  $ dune unstable-fmt <<EOF
  > (a ;comment
  > )
  > (a ;multiline
  > ;comment
  > )
  > EOF
  (a
   ; comment
   )
  
  (a
   ; multiline
   ; comment
   )
