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

  $ dune format-dune-file dune.dune
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

  $ echo '(library (name dune) (libraries unix stdune fiber xdg dune_re threads opam_file_format dune_lang ocaml_config which_program) (synopsis "Internal Dune library, do not use!") (preprocess  (action (run %{project_root}/src/let-syntax/pp.exe %{input-file}))))' | dune format-dune-file --dune-version 3.16
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

The same file, but in the current version:

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

For dune-lang >= 3.22, multi-line block strings are preserved:

  $ dune format-dune-file --dune-version 3.22 <<EOF
  > (echo "\> multi
  >       "\> line
  >       "\> string
  >       "\| string
  > )
  > EOF
  (echo
   "\> multi
   "\> line
   "\> string
   "\| string
   )

For older versions, multi-line strings are escaped:

  $ dune format-dune-file --dune-version 3.21 <<EOF
  > (echo "\> multi
  >       "\> line
  >       "\> string
  >       "\| string
  > )
  > EOF
  (echo "multi\nline\nstring\nstring\n")

Regular quoted strings with embedded newlines are always escaped:

  $ dune format-dune-file <<EOF
  > (echo "\
  > multi
  > line
  > string
  > ")
  > EOF
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

Using the built-in action.

  $ make_dune_project 3.18

  $ cat >dune <<EOF
  > (rule (with-stdout-to file (echo "(   a     c)")))
  > (rule (format-dune-file file file.formatted))
  > EOF

  $ dune build file.formatted

  $ cat _build/default/file.formatted
  (a c)

Version check.

  $ make_dune_project 3.17

  $ dune build file.out
  File "dune", line 2, characters 0-45:
  2 | (rule (format-dune-file file file.formatted))
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: 'format-dune-file' in short-form 'rule' is only available since
  version 3.18 of the dune language. Please update your dune-project file to
  have (lang dune 3.18).
  [1]

  $ cat >dune <<EOF
  > (rule (with-stdout-to file (echo "(   a     c)")))
  > (rule (action (format-dune-file file file.formatted)))
  > EOF

  $ dune build file.out
  File "dune", line 2, characters 14-52:
  2 | (rule (action (format-dune-file file file.formatted)))
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: 'format-dune-file' is only available since version 3.18 of the dune
  language. Please update your dune-project file to have (lang dune 3.18).
  [1]

Behaviour when the dune file is not syntactically valid.

  $ make_dune_project 3.18

  $ cat >dune <<EOF
  > (rule (with-stdout-to file (echo "xxx yyy (")))
  > (rule (format-dune-file file file.formatted))
  > EOF

  $ dune build file.formatted
  File "_build/default/file", line 1, characters 9-9:
  1 | xxx yyy (
               
  Error: unclosed parenthesis at end of input
  [1]

If one does not specify the --version flag, then the version of the current Dune
project is used (if any).

  $ cat >test <<EOF
  > (aaaaaaaaaaa bbbbbbbbbbbbb ccccccccccccccccc dddddddddddddddddd
  > aaaaaaaaaaa bbbbbbbbbbbbb ccccccccccccccccc dddddddddddddddddd)
  > EOF

  $ make_dune_project 2.7

  $ dune format-dune-file <test
  (aaaaaaaaaaa bbbbbbbbbbbbb ccccccccccccccccc dddddddddddddddddd aaaaaaaaaaa
    bbbbbbbbbbbbb ccccccccccccccccc dddddddddddddddddd)

  $ make_dune_project 2.8

  $ dune format-dune-file <test
  (aaaaaaaaaaa
   bbbbbbbbbbbbb
   ccccccccccccccccc
   dddddddddddddddddd
   aaaaaaaaaaa
   bbbbbbbbbbbbb
   ccccccccccccccccc
   dddddddddddddddddd)

When a file is passed as an argument, the version used is that of the project
owning the file (if any). Note that the workspace root when invoking Dune from
within Dune is always the directory directly containing the file.

  $ mkdir -p sub/sub
  $ cat >sub/dune-project <<EOF
  > (lang dune 2.8)
  > EOF
  $ cat >sub/sub/dune-project <<EOF
  > (lang dune 2.7)
  > EOF
  $ cp test sub/
  $ cp test sub/sub/

  $ cd sub
  $ dune format-dune-file sub/test
  Entering directory 'sub'
  (aaaaaaaaaaa bbbbbbbbbbbbb ccccccccccccccccc dddddddddddddddddd aaaaaaaaaaa
    bbbbbbbbbbbbb ccccccccccccccccc dddddddddddddddddd)
  Leaving directory 'sub'

  $ cd sub
  $ dune format-dune-file ../test
  Entering directory '..'
  (aaaaaaaaaaa
   bbbbbbbbbbbbb
   ccccccccccccccccc
   dddddddddddddddddd
   aaaaaaaaaaa
   bbbbbbbbbbbbb
   ccccccccccccccccc
   dddddddddddddddddd)
  Leaving directory '..'

Strings with backslash line continuation lose the continuation during parsing:

  $ dune format-dune-file <<EOF
  > (bash "long command \
  > with continuation")
  > EOF
  (bash "long command with continuation")

Block strings can be used for multi-line bash commands (>= 3.22):

  $ dune format-dune-file --dune-version 3.22 <<EOF
  > (bash "\| long command
  >       "\| with continuation
  > )
  > EOF
  (bash
   "\| long command
   "\| with continuation
   )

Raw block strings preserve special characters:

  $ dune format-dune-file --dune-version 3.22 <<EOF
  > (bash "\> echo "hello"
  >       "\> echo "world"
  > )
  > EOF
  (bash
   "\> echo "hello"
   "\> echo "world"
   )

Long single-line strings are not automatically wrapped:

  $ dune format-dune-file <<EOF
  > (bash "very long command that exceeds 80 characters and should ideally be wrapped somehow but currently just stays on one line")
  > EOF
  (bash
    "very long command that exceeds 80 characters and should ideally be wrapped somehow but currently just stays on one line")

Real-world example with long bash command (stays on one line):

  $ dune format-dune-file <<EOF
  > (rule
  >  (targets jumptbl.h)
  >  (mode fallback)
  >  (deps
  >   (:h instruct.h))
  >  (action
  >   (with-stdout-to
  >    %{targets}
  >    (bash
  >     "cat %{h} | tr -d '\\\\r' | sed -n -e '/^  /s/ \\\\([A-Z]\\\\)/ \\\\&\\\\&lbl_\\\\1/gp' -e '/^}/q'"))))
  > EOF
  (rule
   (targets jumptbl.h)
   (mode fallback)
   (deps
    (:h instruct.h))
   (action
    (with-stdout-to
     %{targets}
     (bash
       "cat %{h} | tr -d '\\r' | sed -n -e '/^  /s/ \\([A-Z]\\)/ \\&\\&lbl_\\1/gp' -e '/^}/q'"))))

Using block strings for long bash commands (>= 3.22) preserves line breaks:

  $ dune format-dune-file --dune-version 3.22 <<EOF
  > (rule
  >  (targets jumptbl.h)
  >  (mode fallback)
  >  (deps
  >   (:h instruct.h))
  >  (action
  >   (with-stdout-to
  >    %{targets}
  >    (bash "\> cat %{h} | tr -d '\r' |
  >          "\>   sed -n -e '/^  /s/ \([A-Z]\)/ \&\&lbl_\1/gp' -e '/^}/q'
  > ))))
  > EOF
  (rule
   (targets jumptbl.h)
   (mode fallback)
   (deps
    (:h instruct.h))
   (action
    (with-stdout-to
     %{targets}
     (bash
      "\> cat %{h} | tr -d '\r' |
      "\>   sed -n -e '/^  /s/ \([A-Z]\)/ \&\&lbl_\1/gp' -e '/^}/q'
      ))))
