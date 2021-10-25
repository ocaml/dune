Tests for directory targets.

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > EOF

Directory targets require an extension.

  $ cat > dune <<EOF
  > (rule
  >   (targets output/*)
  >   (action (bash "true")))
  > EOF

  $ dune build output/x
  File "dune", line 1, characters 0-52:
  1 | (rule
  2 |   (targets output/*)
  3 |   (action (bash "true")))
  Error: Directory targets require the 'directory-targets' extension
  [1]

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > (using directory-targets 0.1)
  > EOF

Directory targets are not allowed for non-sandboxed rules.

  $ dune build output/x
  File "dune", line 1, characters 0-52:
  1 | (rule
  2 |   (targets output/*)
  3 |   (action (bash "true")))
  Error: Rules with directory targets must be sandboxed
  [1]

Ensure directory targets are produced.

  $ cat > dune <<EOF
  > (rule
  >   (deps (sandbox always))
  >   (targets output/*)
  >   (action (bash "true")))
  > EOF

  $ dune build output/x
  File "dune", line 1, characters 0-78:
  1 | (rule
  2 |   (deps (sandbox always))
  3 |   (targets output/*)
  4 |   (action (bash "true")))
  Error: Rule failed to produce directory "output"
  [1]

Error message when the matching directory target doesn't contain a requested path.

  $ cat > dune <<EOF
  > (rule
  >   (deps (sandbox always))
  >   (targets output/*)
  >   (action (bash "mkdir output")))
  > EOF

  $ dune build output/x
  File "dune", line 1, characters 0-86:
  1 | (rule
  2 |   (deps (sandbox always))
  3 |   (targets output/*)
  4 |   (action (bash "mkdir output")))
  Error: This rule defines a directory target "output" that matches the
  requested path "output/x" but the rule's action didn't produce it
  [1]

Build directory target from the command line.

  $ cat > dune <<EOF
  > (rule
  >   (deps (sandbox always))
  >   (targets output/*)
  >   (action (bash "mkdir output; echo x > output/x; echo y > output/y")))
  > EOF

  $ dune build output/x
  $ cat _build/default/output/x
  x
  $ cat _build/default/output/y
  y

Requesting the directory target directly works too.

  $ cat > dune <<EOF
  > (rule
  >   (deps src_x (sandbox always))
  >   (targets output/*)
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

Print rules.

  $ dune rules output
  ((deps
    ((File (In_build_dir _build/default/src_x))
     (Sandbox_config ((disallow none)))
     (Sandbox_config ())))
   (targets ((In_build_dir _build/default/output/*)))
   (context default)
   (action
    (chdir
     _build/default
     (bash "mkdir output; cat src_x > output/x; echo y > output/y"))))

  $ dune rules -m output | tr '\t' ' ' | head -n -1
  _build/default/output: _build/default/src_x
   mkdir -p _build/default; \
   mkdir -p _build/default; \
   cd _build/default; \
   bash -e -u -o pipefail -c \
     'mkdir output; cat src_x > output/x; echo y > output/y'

Error when requesting a missing subdirectory of a directory target.

  $ cat > dune <<EOF
  > (rule
  >   (deps (sandbox always))
  >   (targets output/*)
  >   (action (bash "mkdir output; echo x > output/x; echo y > output/y")))
  > EOF

Error message when depending on a file that isn't produced by the matching
directory target.

  $ cat > dune <<EOF
  > (rule
  >   (deps (sandbox always))
  >   (targets output/*)
  >   (action (bash "mkdir -p output/subdir; echo a > output/a; echo b > output/subdir/b")))
  > (rule
  >   (deps output/subdir/c)
  >   (target main)
  >   (action (bash "cat output/subdir/c > main")))
  > EOF

  $ dune build main
  File "dune", line 1, characters 0-141:
  1 | (rule
  2 |   (deps (sandbox always))
  3 |   (targets output/*)
  4 |   (action (bash "mkdir -p output/subdir; echo a > output/a; echo b > output/subdir/b")))
  Error: This rule defines a directory target "output" that matches the
  requested path "output/subdir/c" but the rule's action didn't produce it
  -> required by _build/default/main
  [1]

Depend on a file from a directory target.

  $ cat > dune <<EOF
  > (rule
  >   (deps (sandbox always))
  >   (targets output/*)
  >   (action (bash "mkdir -p output/subdir; echo a > output/a; echo b > output/subdir/b")))
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
  >   (targets output/*)
  >   (action (bash "mkdir -p output/subdir; echo a > output/a; echo b > output/b; echo c > output/subdir/c")))
  > (rule
  >   (deps (glob_files output/*))
  >   (target level1)
  >   (action (bash "echo %{deps}; ls output > level1")))
  > (rule
  >   (deps (glob_files output/subdir/*))
  >   (target level2)
  >   (action (bash "echo %{deps}; ls output/subdir > level2")))
  > EOF

Note that %{deps} currently expands only to the statically known paths, which is
just the "output" directory and doesn't include the contained generated files.

# CR-someday amokhov: Improve this so that %{deps} includes generated files.

  $ dune build level1
          bash level1
  output

  $ cat _build/default/level1
  a
  b
  subdir

Again, %{deps} currently expands to "output/subdir" instead of "output/subdir/c"

  $ dune build level2
          bash level2
  output/subdir
  $ cat _build/default/level2
  c

Depending on a directory target directly (rather than on individual files) works
too. Note that this can be achieved in two ways:

(1) By depending on the recursively computed digest the directory's contents;

(2) By depending on the mtime of the directory.

Currently Dune implements (2) but we'd like to switch to (1) because it supports
the early cutoff optimisation and is also more reliable.

The [src_c] dependency is unused in the rule's action but we use it to force the
rule to rerun when needed.

  $ cat > dune <<EOF
  > (rule
  >   (deps src_a src_b src_c (sandbox always))
  >   (targets output/*)
  >   (action (bash "echo running; mkdir -p output/subdir; cat src_a > output/a; cat src_b > output/subdir/b")))
  > (rule
  >   (deps output)
  >   (target contents)
  >   (action (bash "echo running; echo 'a:' > contents; cat output/a >> contents; echo 'b:' >> contents; cat output/subdir/b >> contents")))
  > EOF

  $ echo a > src_a
  $ echo b > src_b
  $ echo c > src_c
  $ dune build contents
          bash output
  running
          bash contents
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
          bash output
  running
          bash contents
  running
  $ cat _build/default/contents
  a:
  a
  b:
  new-b

There is no early cutoff on directory targets at the moment. Ideally, we should
skip the second action since the produced directory has the same contents.

  $ echo new-cc > src_c
  $ dune build contents
          bash output
  running
          bash contents
  running
  $ cat _build/default/contents
  a:
  a
  b:
  new-b

There is no shared cache support for directory targets at the moment. Note that
we rerun both actions: the first one because there is no shared cache support
and the second one because of the lack of early cutoff.

  $ rm _build/default/output/a
  $ dune build contents
          bash output
  running
          bash contents
  running

Check that Dune clears stale files from directory targets.

  $ cat > dune <<EOF
  > (rule
  >   (deps src_a src_b src_c (sandbox always))
  >   (targets output/*)
  >   (action (bash "echo running; mkdir -p output/subdir; cat src_a > output/new-a; cat src_b > output/subdir/b")))
  > (rule
  >   (deps output)
  >   (target contents)
  >   (action (bash "echo running; echo 'new-a:' > contents; cat output/new-a >> contents; echo 'b:' >> contents; cat output/subdir/b >> contents")))
  > EOF

  $ dune build contents
          bash output
  running
          bash contents
  running

Note that the stale "output/a" file got removed.

  $ ls _build/default/output | sort
  new-a
  subdir

Directory target whose name conflicts with an internal directory used by Dune.

  $ cat > dune <<EOF
  > (rule
  >   (deps (sandbox always))
  >   (targets .dune/*)
  >   (action (bash "mkdir .dune; echo hello > .dune/hello")))
  > EOF

  $ dune build .dune/hello
  File "dune", line 1, characters 0-110:
  1 | (rule
  2 |   (deps (sandbox always))
  3 |   (targets .dune/*)
  4 |   (action (bash "mkdir .dune; echo hello > .dune/hello")))
  Error: This rule defines a directory target ".dune" whose name conflicts with
  an internal directory used by Dune. Please use a different name.
  -> required by _build/default/.dune/hello
  [1]
