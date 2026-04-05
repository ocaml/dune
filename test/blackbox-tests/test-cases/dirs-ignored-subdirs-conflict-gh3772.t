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
  File "dune", line 2, characters 6-6:
  2 | (dirs)
            
  Error: Cannot have both dirs and ignored_subdirs stanza in a dune file. 
  [1]
