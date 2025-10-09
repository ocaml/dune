Test validation of symlinks that escape the directory target.

Symlinks that point outside the directory target boundary are forbidden
for reproducibility and sandboxing concerns.

  $ cat > dune-project << EOF
  > (lang dune 3.21)
  > (using directory-targets 0.1)
  > EOF

Symlinks escaping to sibling directories are now forbidden:

  $ cat > dune << EOF
  > (rule
  >  (target (dir sibling))
  >  (action
  >   (progn
  >    (run mkdir -p sibling)
  >    (system "echo content > sibling/file.txt"))))
  > 
  > (rule
  >  (target (dir d))
  >  (deps sibling)
  >  (action
  >   (progn
  >    (run mkdir -p d)
  >    (chdir d
  >     (run ln -s ../sibling escape)))))
  > EOF

  $ dune build d
  File "dune", lines 8-15, characters 0-127:
   8 | (rule
   9 |  (target (dir d))
  10 |  (deps sibling)
  11 |  (action
  12 |   (progn
  13 |    (run mkdir -p d)
  14 |    (chdir d
  15 |     (run ln -s ../sibling escape)))))
  Error: Error trying to read targets after a rule was run:
  - d/escape: Unexpected file kind "S_DIR" (directory)
  [1]

Symlinks escaping to source tree are also forbidden:

  $ mkdir source_dir
  $ touch source_dir/file.txt

  $ cat > dune << EOF
  > (rule
  >  (target (dir d3))
  >  (deps source_dir/file.txt)
  >  (action
  >   (progn
  >    (run mkdir -p d3)
  >    (chdir d3
  >     (run ln -s ../../../source_dir escape_to_source)))))
  > EOF

  $ dune build d3
  File "dune", lines 1-8, characters 0-161:
  1 | (rule
  2 |  (target (dir d3))
  3 |  (deps source_dir/file.txt)
  4 |  (action
  5 |   (progn
  6 |    (run mkdir -p d3)
  7 |    (chdir d3
  8 |     (run ln -s ../../../source_dir escape_to_source)))))
  Error: Error trying to read targets after a rule was run:
  - d3/escape_to_source: Unexpected file kind "S_DIR" (directory)
  [1]

Symlinks escaping to other build targets are also forbidden:

Create a directory target to link to:

  $ cat > dune << EOF
  > (rule
  >  (target (dir outside_dir))
  >  (action
  >   (progn
  >    (run mkdir -p outside_dir)
  >    (system "echo 'content' > outside_dir/external.txt"))))
  > 
  > (rule
  >  (target (dir d_outside))
  >  (deps outside_dir)
  >  (action
  >   (progn
  >    (run mkdir -p d_outside)
  >    (chdir d_outside
  >     (run ln -s ../outside_dir link_to_outside)))))
  > EOF

  $ dune build d_outside
  File "dune", lines 8-15, characters 0-168:
   8 | (rule
   9 |  (target (dir d_outside))
  10 |  (deps outside_dir)
  11 |  (action
  12 |   (progn
  13 |    (run mkdir -p d_outside)
  14 |    (chdir d_outside
  15 |     (run ln -s ../outside_dir link_to_outside)))))
  Error: Error trying to read targets after a rule was run:
  - d_outside/link_to_outside: Unexpected file kind "S_DIR" (directory)
  [1]

Absolute path symlinks are also forbidden:

Create a test directory for absolute path tests:

  $ mkdir -p abs_target_test
  $ cat > abs_target_test/file.txt << EOF
  > content
  > EOF

Test absolute symlink to directory inside the project:

  $ cat > dune << EOF
  > (rule
  >  (target (dir d_abs1))
  >  (action
  >   (progn
  >    (run mkdir -p d_abs1)
  >    (run ln -s $PWD/abs_target_test d_abs1/abs_symlink))))
  > EOF

  $ dune build d_abs1 2>&1 | tail -2
  Error: Error trying to read targets after a rule was run:
  - d_abs1/abs_symlink: Unexpected file kind "S_DIR" (directory)


