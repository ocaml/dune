Tests for directory targets.

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

Directory targets require an extension.

  $ cat > dune <<EOF
  > (rule
  >   (targets (dir output))
  >   (action (bash "true")))
  > EOF

  $ dune build output/x
  File "dune", line 2, characters 16-22:
  2 |   (targets (dir output))
                      ^^^^^^
  Error: Directory targets require the 'directory-targets' extension
  [1]

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > (using directory-targets 0.1)
  > EOF

Ensure directory targets are produced.

  $ cat > dune <<EOF
  > (rule
  >   (deps (sandbox always))
  >   (targets (dir output))
  >   (action (bash "true")))
  > EOF

  $ dune build output/x
  File "dune", line 1, characters 0-82:
  1 | (rule
  2 |   (deps (sandbox always))
  3 |   (targets (dir output))
  4 |   (action (bash "true")))
  Error: Rule failed to produce directory "output"
  [1]

Error message when the matching directory target doesn't contain a requested path.

  $ cat > dune <<EOF
  > (rule
  >   (deps (sandbox always))
  >   (targets (dir output))
  >   (action (bash "mkdir output")))
  > EOF

  $ dune build output/x
  File "dune", line 1, characters 0-90:
  1 | (rule
  2 |   (deps (sandbox always))
  3 |   (targets (dir output))
  4 |   (action (bash "mkdir output")))
  Error: This rule defines a directory target "output" that matches the
  requested path "output/x" but the rule's action didn't produce it
  [1]

Build directory target from the command line.

  $ cat > dune <<EOF
  > (rule
  >   (deps (sandbox always))
  >   (targets (dir output))
  >   (action (bash "mkdir output; echo x > output/x; echo y > output/y")))
  > EOF

  $ dune build output/x
  $ cat _build/default/output/x
  x
  $ cat _build/default/output/y
  y

Test that workspace-local cache works for directory targets.

  $ dune build output/x --debug-cache=workspace-local

Requesting the directory target directly works too.

  $ cat > dune <<EOF
  > (rule
  >   (deps src_x (sandbox always))
  >   (targets (dir output))
  >   (action (bash "mkdir output; cat src_x > output/x; echo y > output/y")))
  > EOF

  $ rm -rf _build
  $ echo x > src_x
  $ dune build output
  $ cat _build/default/output/x
  x
  $ cat _build/default/output/y
  y

Rebuilding works correctly.

  $ echo new-x > src_x
  $ dune build output
  $ cat _build/default/output/x
  new-x

Hints for directory targets.

  $ dune build outputs
  Error: Don't know how to build outputs
  Hint: did you mean output?
  [1]

Print rules:

  $ dune rules -m output | tr '\t' ' '
  _build/default/output: _build/default/src_x
   mkdir -p _build/default; \
   mkdir -p _build/default; \
   cd _build/default; \
   bash -e -u -o pipefail -c \
     'mkdir output; cat src_x > output/x; echo y > output/y'

  $ dune rules output
  ((deps ((File (In_build_dir _build/default/src_x))))
   (targets ((files ()) (directories (default/output))))
   (context default)
   (action
    (chdir
     _build/default
     (bash "mkdir output; cat src_x > output/x; echo y > output/y"))))

Error when requesting a missing subdirectory of a directory target.

  $ cat > dune <<EOF
  > (rule
  >   (deps (sandbox always))
  >   (targets (dir output))
  >   (action (bash "mkdir output; echo x > output/x; echo y > output/y")))
  > EOF

  $ dune build output/subdir
  File "dune", line 1, characters 0-128:
  1 | (rule
  2 |   (deps (sandbox always))
  3 |   (targets (dir output))
  4 |   (action (bash "mkdir output; echo x > output/x; echo y > output/y")))
  Error: This rule defines a directory target "output" that matches the
  requested path "output/subdir" but the rule's action didn't produce it
  [1]

Error message when depending on a file that isn't produced by the matching
directory target.

  $ cat > dune <<EOF
  > (rule
  >   (deps (sandbox always))
  >   (targets (dir output))
  >   (action (bash "\| mkdir -p output/subdir;
  >                 "\| echo a > output/a;
  >                 "\| echo b > output/subdir/b
  > )))
  > (rule
  >   (deps output/subdir/c)
  >   (target main)
  >   (action (bash "cat output/subdir/c > main")))
  > EOF

  $ dune build main
  File "dune", line 1, characters 0-188:
  1 | (rule
  2 |   (deps (sandbox always))
  3 |   (targets (dir output))
  4 |   (action (bash "\| mkdir -p output/subdir;
  5 |                 "\| echo a > output/a;
  6 |                 "\| echo b > output/subdir/b
  7 | )))
  Error: This rule defines a directory target "output" that matches the
  requested path "output/subdir/c" but the rule's action didn't produce it
  -> required by _build/default/main
  [1]

Depend on a file from a directory target.

  $ cat > dune <<EOF
  > (rule
  >   (deps (sandbox always))
  >   (targets (dir output))
  >   (action (bash "\| mkdir -p output/subdir;
  >                 "\| echo a > output/a;
  >                 "\| echo b > output/subdir/b
  > )))
  > (rule
  >   (deps output/subdir/b)
  >   (target main)
  >   (action (bash "cat output/subdir/b > main; echo 2 >> main")))
  > EOF

  $ dune build main
  $ cat _build/default/main
  b
  2
  $ cat _build/default/output/a
  a
  $ cat _build/default/output/subdir/b
  b

Interaction of globs and directory targets.

  $ cat > dune <<EOF
  > (rule
  >   (deps (sandbox always))
  >   (targets (dir output))
  >   (action (bash "\| mkdir -p output/subdir;
  >                 "\| echo a > output/a.txt;
  >                 "\| echo b > output/b.txt;
  >                 "\| echo c > output/c;
  >                 "\| echo d > output/subdir/d.txt;
  >                 "\| echo e > output/subdir/e
  > )))
  > (rule
  >   (deps (glob_files output/*.txt))
  >   (target level1)
  >   (action (bash "echo %{deps}; ls output > level1")))
  > (rule
  >   (deps (glob_files output/subdir/*))
  >   (target level2)
  >   (action (bash "echo %{deps}; ls output/subdir > level2")))
  > EOF

Note: %{deps} expands to the set of generated files that match the glob [*.txt],
however, the action currently has access to all of the paths, along with any of
the subdirectories included into the directory target.

# CR-someday amokhov: Remove the files that action didn't depend on.

  $ dune build level1
  output/a.txt output/b.txt

  $ cat _build/default/level1
  a.txt
  b.txt
  c
  subdir

Depending on a glob in a subdirectory of a directory target works too.

  $ dune build level2
  output/subdir/d.txt output/subdir/e
  $ cat _build/default/level2
  d.txt
  e

Depending on a directory target directly (rather than on individual files) works
too. Note that this can be achieved in two ways:

(1) By depending on the recursively computed digest of the directory's contents;

(2) By depending on the mtime of the directory.

Currently Dune implements (2) but we'd like to switch to (1) because it supports
the early cutoff optimisation and is also more reliable.

The [src_c] dependency is unused in the rule's action but we use it to force the
rule to rerun when needed.

# CR-someday amokhov: Right now we accept simply "output" as a dependency
# specification, which is inconsistent with the target specification. This
# should be fixed, i.e. we should require "(dir output)" instead.

  $ cat > dune <<EOF
  > (rule
  >   (deps src_a src_b src_c (sandbox always))
  >   (targets (dir output))
  >   (action (bash "\| echo running;
  >                 "\| mkdir -p output/subdir;
  >                 "\| cat src_a > output/a;
  >                 "\| cat src_b > output/subdir/b
  > )))
  > (rule
  >   (deps output)
  >   (target contents)
  >   (action (bash "echo running; echo 'a:' > contents; cat output/a >> contents; echo 'b:' >> contents; cat output/subdir/b >> contents")))
  > EOF

  $ echo a > src_a
  $ echo b > src_b
  $ echo c > src_c
  $ dune build contents
  running
  running
  $ cat _build/default/contents
  a:
  a
  b:
  b

We wait for the file system's clock to advance to make sure the directory's
mtime changes when the rule reruns. We can delete this when switching to (1).

  $ dune_cmd wait-for-fs-clock-to-advance
  $ echo new-b > src_b

  $ dune build contents
  running
  running
  $ cat _build/default/contents
  a:
  a
  b:
  new-b

Dune supports early cutoff for directory targets: it skips the second action
since the produced directory has the same contents.

  $ dune_cmd wait-for-fs-clock-to-advance
  $ echo new-cc > src_c
  $ dune build contents
  running
  $ cat _build/default/contents
  a:
  a
  b:
  new-b

There is no shared cache support for directory targets at the moment, which is
why we rerun the first action.

  $ dune_cmd wait-for-fs-clock-to-advance
  $ rm _build/default/output/a
  $ dune build contents
  running

Check that Dune clears stale files from directory targets.

  $ cat > dune <<EOF
  > (rule
  >   (deps src_a src_b src_c (sandbox always))
  >   (targets (dir output))
  >   (action (bash "\| echo running;
  >                 "\| mkdir -p output/subdir;
  >                 "\| cat src_a > output/new-a;
  >                 "\| cat src_b > output/subdir/b
  > )))
  > (rule
  >   (deps output)
  >   (target contents)
  >   (action (bash "echo running; echo 'new-a:' > contents; cat output/new-a >> contents; echo 'b:' >> contents; cat output/subdir/b >> contents")))
  > EOF

  $ dune build contents
  running
  running

Note that the stale "output/a" file got removed.

  $ ls _build/default/output | sort
  new-a
  subdir

Directory target whose name conflicts with an internal directory used by Dune.

  $ cat > dune <<EOF
  > (rule
  >   (deps (sandbox always))
  >   (targets (dir .dune))
  >   (action (bash "mkdir .dune; echo hello > .dune/hello")))
  > EOF

  $ dune build .dune/hello
  File "dune", line 1, characters 0-114:
  1 | (rule
  2 |   (deps (sandbox always))
  3 |   (targets (dir .dune))
  4 |   (action (bash "mkdir .dune; echo hello > .dune/hello")))
  Error: This rule defines a target ".dune" whose name conflicts with an
  internal directory used by Dune. Please use a different name.
  [1]

Multi-component target directories are not allowed.

  $ cat > dune <<EOF
  > (rule
  >   (deps (sandbox always))
  >   (targets (dir output/subdir))
  >   (action (bash "mkdir output; echo x > output/x; echo y > output/y")))
  > EOF

  $ dune build output/x
  File "dune", line 3, characters 16-29:
  3 |   (targets (dir output/subdir))
                      ^^^^^^^^^^^^^
  Error: Directory targets must have exactly one path component.
  [1]

File and directory target with the same name.

  $ cat > dune <<EOF
  > (rule
  >   (deps (sandbox always))
  >   (targets output (dir output))
  >   (action (bash "mkdir output; echo x > output/x; echo y > output/y")))
  > EOF

  $ dune build output/x
  File "dune", line 1, characters 0-135:
  1 | (rule
  2 |   (deps (sandbox always))
  3 |   (targets output (dir output))
  4 |   (action (bash "mkdir output; echo x > output/x; echo y > output/y")))
  Error: "output" is declared as both a file and a directory target.
  [1]

File and directory target with the same name, defined in different rules.

  $ cat > dune <<EOF
  > (rule
  >   (targets output)
  >   (action (bash "echo hi > output")))
  > (rule
  >   (deps (sandbox always))
  >   (targets (dir output))
  >   (action (bash "mkdir output; echo x > output/x; echo y > output/y")))
  > EOF

  $ dune build output/x
  Error: Multiple rules generated for _build/default/output:
  - dune:1
  - dune:4
  [1]

Conflict between a target directory and a source directory.

  $ cat > dune <<EOF
  > (rule
  >   (deps (sandbox always))
  >   (targets (dir output))
  >   (action (bash "mkdir output; echo x > output/x; echo y > output/y")))
  > EOF

  $ mkdir output
  $ dune build output/x
  File "dune", line 1, characters 0-128:
  1 | (rule
  2 |   (deps (sandbox always))
  3 |   (targets (dir output))
  4 |   (action (bash "mkdir output; echo x > output/x; echo y > output/y")))
  Error: This rule defines a target "output" whose name conflicts with a source
  directory in the same directory.
  Hint: If you want Dune to generate and replace "output", add (mode promote)
  to the rule stanza. Alternatively, you can delete "output" from the source
  tree or change the rule to generate a different target.
  [1]

Conflict between a target directory and a source file.

  $ cat > dune <<EOF
  > (rule
  >   (deps (sandbox always))
  >   (targets (dir output))
  >   (action (bash "mkdir output; echo x > output/x; echo y > output/y")))
  > EOF

  $ rm -rf output
  $ touch output
  $ dune build output/x
  Error: Multiple rules generated for _build/default/output:
  - file present in source tree
  - dune:1
  Hint: rm -f output
  [1]
