Reproduction of https://github.com/ocaml/dune/issues/3773

Mixing incompatible features should fail earlier with a proper message.

  $ cat > dune-project <<EOF
  > (lang dune 2.0)
  > EOF

  $ cat > dune <<EOF
  > (ignored_subdirs (node_modules))
  > (dirs)
  > (data_only_dirs)
  > EOF

  $ dune build
  File "dune", line 1, characters 17-31:
  1 | (ignored_subdirs (node_modules))
                       ^^^^^^^^^^^^^^
  Warning: ignored_subdirs is deprecated in 1.6. Use dirs to specify visible
  directories or data_only_dirs for ignoring only dune files.
  File "src/dune_engine/sub_dirs.ml", line 93, characters 33-39:
  File "src/dune_engine/sub_dirs.ml", line 93, characters 33-39: Assertion
  failed
  Raised at file "src/dune_engine/sub_dirs.ml", line 93, characters 33-45
  Called from file "src/dune_engine/sub_dirs.ml", line 302, characters 11-65
  Called from file "src/dune_lang/decoder.ml", line 92, characters 3-6
  Called from file "src/dune_lang/decoder.ml", line 616, characters 21-53
  Called from file "src/dune_lang/decoder.ml", line 316, characters 19-28
  Called from file "src/dune_lang/decoder.ml" (inlined), line 236, characters
    22-51
  Called from file "src/dune_lang/decoder.ml", line 312, characters 2-242
  Called from file "src/dune_lang/decoder.ml", line 185, characters 13-29
  Called from file "src/dune_engine/file_tree.ml", line 87, characters 8-103
  Called from file "src/dune_engine/file_tree.ml", line 108, characters 22-66
  Called from file "src/stdune/exn.ml", line 12, characters 8-11
  Re-raised at file "src/stdune/exn.ml", line 18, characters 4-11
  Called from file "src/dune_engine/file_tree.ml", line 102, characters 8-380
  Called from file "src/dune_engine/file_tree.ml", line 517, characters 11-67
  Called from file "src/dune_engine/file_tree.ml", line 574, characters 6-63
  Called from file "src/stdune/exn_with_backtrace.ml", line 9, characters 8-12
  Re-raised at file "src/stdune/exn.ml", line 36, characters 27-56
  Called from file "src/dune_engine/file_tree.ml", line 647, characters 39-74
  Called from file "src/dune_engine/file_tree.ml", line 650, characters 33-60
  Called from file "src/dune_engine/file_tree.ml" (inlined), line 653,
    characters 14-30
  Called from file "src/dune_engine/file_tree.ml", line 761, characters 13-20
  Called from file "src/dune_rules/dune_load.ml", line 247, characters 4-141
  Called from file "src/dune_rules/main.ml", line 46, characters 13-41
  Called from file "bin/import.ml", line 88, characters 21-42
  Called from file "src/fiber/fiber.ml", line 193, characters 9-14
  
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  [1]
