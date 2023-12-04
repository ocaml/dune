Version 0.1 is deleted.

  $ cat > dune-project << EOF
  > (lang dune 3.4)
  > (using ctypes 0.1)
  > EOF

  $ dune build
  File "dune-project", line 2, characters 14-17:
  2 | (using ctypes 0.1)
                    ^^^
  Error: Version 0.1 of the ctypes extension has been deleted in Dune 3.11.
  Please port this project to a newer version of the extension, such as 0.3.
  Hint: You will also need to upgrade to (lang dune 3.7).
  [1]
