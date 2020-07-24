  $ cat >using-mdx/dune-project <<EOF
  > (lang dune 2.4)
  > EOF

To use the mdx stanza you need to explicitly set (using mdx ..) in the
dune-project

  $ dune build @install --root using-mdx/
  Entering directory 'using-mdx'
  Info: Appending this line to dune-project: (using mdx 0.1)

It also requires dune lang 2.4 or higher

  $ dune build @install --root lang-version/
  Entering directory 'lang-version'
  File "dune-project", line 3, characters 11-14:
  3 | (using mdx 0.1)
                 ^^^
  Warning: Version 0.1 of mdx extension to verify code blocks in .md files is
  not supported until version 2.4 of the dune language.
  There are no supported versions of this extension in version 2.3 of the dune
  language.
  
  File "dune", line 1, characters 0-5:
  1 | (mdx)
      ^^^^^
  Error: 'mdx' is only available since version 2.4 of the dune language. Please
  update your dune-project file to have (lang dune 2.4).
  [1]

You can use the mdx stanza to check your documentation in markdown and mli files

  $ dune runtest --root simple/
  Entering directory 'simple'
  File "README.md", line 1, characters 0-0:
  Error: Files _build/default/README.md and _build/default/README.md.corrected
  differ.
  [1]

Dune should invoke `ocaml-mdx deps` to figure out the files and directories a markdown
or mli to-be-mdxed file depends upon

  $ dune runtest --root mdx-deps/
  Entering directory 'mdx-deps'

You can make local packages available to mdx by using the `packages` field of
the stanza

  $ dune runtest --root local-package
  Entering directory 'local-package'

Even if the packages is unrelated:

  $ cd local-package-unrelated && dune runtest -p unrelated-package; cd ../
  Error: exception { exn = ("Map.find_exn: failed to find key", { key = 2; keys
  = [ 1 ] })
  ; backtrace =
      [ { ocaml =
            "Raised at file \"src/stdune/code_error.ml\", line 9, characters
  30-62\n\
             Called from file \"src/dune/mdx.ml\", line 187, characters
  23-61\n\
             Called from file \"list.ml\", line 103, characters 22-25\n\
             Called from file \"src/stdune/list.ml\", line 5, characters
  19-33\n\
             Called from file \"src/dune/mdx.ml\", line 185, characters
  6-196\n\
             Called from file \"list.ml\", line 110, characters 12-15\n\
             Called from file \"src/dune/gen_rules.ml\", line 128, characters
  6-34\n\
             Called from file \"src/dune/gen_rules.ml\", line 135, characters
  6-96\n\
             Called from file \"list.ml\", line 121, characters 24-34\n\
             Called from file \"src/dune/gen_rules.ml\", line 138, characters
  4-112\n\
             Called from file \"src/dune/gen_rules.ml\", line 218, characters
  4-119\n\
             Called from file \"src/dune/gen_rules.ml\", line 349, characters
  24-59\n\
             Called from file \"src/stdune/exn.ml\", line 12, characters
  8-11\n\
             Re-raised at file \"src/stdune/exn.ml\", line 18, characters
  4-11\n\
             Called from file \"src/memo/implicit_output.ml\", line 120,
  characters 4-162\n\
             Called from file \"src/dune/rules.ml\" (inlined), line 192,
  characters 20-71\n\
             Called from file \"src/dune/rules.ml\", line 195, characters
  20-33\n\
             Called from file \"src/dune/build_system.ml\", line 900,
  characters 6-76\n\
             Called from file \"src/stdune/exn_with_backtrace.ml\", line 9,
  characters 8-12\n\
             "
        ; memo = ("load-dir", In_build_dir "default")
        }
      ; { ocaml =
            "Raised at file \"src/stdune/code_error.ml\", line 9, characters
  30-62\n\
             Called from file \"src/dune/mdx.ml\", line 187, characters
  23-61\n\
             Called from file \"list.ml\", line 103, characters 22-25\n\
             Called from file \"src/stdune/list.ml\", line 5, characters
  19-33\n\
             Called from file \"src/dune/mdx.ml\", line 185, characters
  6-196\n\
             Called from file \"list.ml\", line 110, characters 12-15\n\
             Called from file \"src/dune/gen_rules.ml\", line 128, characters
  6-34\n\
             Called from file \"src/dune/gen_rules.ml\", line 135, characters
  6-96\n\
             Called from file \"list.ml\", line 121, characters 24-34\n\
             Called from file \"src/dune/gen_rules.ml\", line 138, characters
  4-112\n\
             Called from file \"src/dune/gen_rules.ml\", line 218, characters
  4-119\n\
             Called from file \"src/dune/gen_rules.ml\", line 349, characters
  24-59\n\
             Called from file \"src/stdune/exn.ml\", line 12, characters
  8-11\n\
             Re-raised at file \"src/stdune/exn.ml\", line 18, characters
  4-11\n\
             Called from file \"src/memo/implicit_output.ml\", line 120,
  characters 4-162\n\
             Called from file \"src/dune/rules.ml\" (inlined), line 192,
  characters 20-71\n\
             Called from file \"src/dune/rules.ml\", line 195, characters
  20-33\n\
             Called from file \"src/dune/build_system.ml\", line 900,
  characters 6-76\n\
             Called from file \"src/stdune/exn_with_backtrace.ml\", line 9,
  characters 8-12\n\
             Re-raised at file \"src/stdune/exn.ml\", line 36, characters
  27-56\n\
             Called from file \"src/dune/build_system.ml\", line 1046,
  characters 12-43\n\
             Called from file \"src/stdune/exn_with_backtrace.ml\", line 9,
  characters 8-12\n\
             "
        ; memo = ("load-dir", In_build_dir ".aliases/default")
        }
      ]
  ; outer_call_stack = []
  }
  Raised at file "src/stdune/code_error.ml", line 9, characters 30-62
  Called from file "src/dune/mdx.ml", line 187, characters 23-61
  Called from file "list.ml", line 103, characters 22-25
  Called from file "src/stdune/list.ml", line 5, characters 19-33
  Called from file "src/dune/mdx.ml", line 185, characters 6-196
  Called from file "list.ml", line 110, characters 12-15
  Called from file "src/dune/gen_rules.ml", line 128, characters 6-34
  Called from file "src/dune/gen_rules.ml", line 135, characters 6-96
  Called from file "list.ml", line 121, characters 24-34
  Called from file "src/dune/gen_rules.ml", line 138, characters 4-112
  Called from file "src/dune/gen_rules.ml", line 218, characters 4-119
  Called from file "src/dune/gen_rules.ml", line 349, characters 24-59
  Called from file "src/stdune/exn.ml", line 12, characters 8-11
  Re-raised at file "src/stdune/exn.ml", line 18, characters 4-11
  Called from file "src/memo/implicit_output.ml", line 120, characters 4-162
  Called from file "src/dune/rules.ml" (inlined), line 192, characters 20-71
  Called from file "src/dune/rules.ml", line 195, characters 20-33
  Called from file "src/dune/build_system.ml", line 900, characters 6-76
  Called from file "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at file "src/stdune/exn.ml", line 36, characters 27-56
  Called from file "src/dune/build_system.ml", line 1046, characters 12-43
  Called from file "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at file "src/stdune/exn.ml", line 36, characters 27-56
  Called from file "src/dune/build_system.ml", line 685, characters 10-23
  Called from file "src/dune/build_system.ml", line 682, characters 17-34
  Called from file "src/dune/build.ml", line 293, characters 9-22
  Called from file "src/dune/build.ml", line 284, characters 58-73
  Called from file "src/dune/build.ml", line 284, characters 42-57
  Called from file "src/dune/build.ml", line 284, characters 42-57
  Called from file "src/dune/build.ml", line 284, characters 58-73
  Called from file "src/dune/build_system.ml", line 1237, characters 24-39
  Called from file "src/dune/build_system.ml", line 1850, characters 8-97
  Called from file "src/fiber/fiber.ml", line 109, characters 10-15
  Re-raised at file "src/stdune/exn.ml", line 36, characters 27-56
  Called from file "src/fiber/fiber.ml", line 80, characters 10-17
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.

You can set MDX preludes using the preludes field of the stanza

  $ dune runtest --root preludes
  Entering directory 'preludes'
