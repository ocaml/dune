  $ cat > dune-project << EOF
  > (lang dune 2.0)
  > (using action-plugin 0.1)
  > EOF

  $ cat > dune << EOF
  > (data_only_dirs some_dir)
  > \
  > (rule
  >  (alias runtest)
  >  (action (dynamic-run ./foo.exe)))
  > EOF

  $ mkdir some_dir
  $ touch some_dir/some_file1
  $ touch some_dir/some_file2

  $ cp ./bin/foo.exe ./

  $ dune runtest
  Directory listing: [some_file1; some_file2]

A direct run includes directories even though glob dependencies only track
files.

  $ mkdir some_dir/subdir
  $ ./foo.exe
  Directory listing: [some_file1; some_file2; subdir]

A missing directory has an empty listing when the plugin runs directly.

  $ rm -rf some_dir
  $ ./foo.exe
  Directory listing: []

A directory dependency builds its generating rule. Dune rejects directory
targets containing no files, rather than silently returning an empty listing.

  $ cat > dune-project << EOF
  > (lang dune 3.24)
  > (using action-plugin 0.1)
  > EOF
  $ cat > dune << EOF
  > (rule
  >  (targets (dir some_dir))
  >  (action (run mkdir some_dir)))
  > \
  > (rule
  >  (alias check-empty)
  >  (action (dynamic-run ./foo.exe)))
  > EOF
  $ dune build @check-empty
  File "dune", lines 4-6, characters 0-61:
  4 | (rule
  5 |  (alias check-empty)
  6 |  (action (dynamic-run ./foo.exe)))
  Rule produced directory "some_dir" that contains no files nor
  non-empty subdirectories
  [1]

A dynamic action may be part of a rule with a directory target. The action
plugin does not manage the rule's targets.

  $ cat > dune << EOF
  > (data_only_dirs some_dir)
  > \
  > (rule
  >  (targets (dir output))
  >  (action
  >   (progn
  >    (dynamic-run ./foo.exe)
  >    (run mkdir output)
  >    (run touch output/file))))
  > EOF
  $ dune build output
  Directory listing: []
