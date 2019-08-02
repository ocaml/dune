  $ echo '(lang dune 2.0)' > dune-project
  $ echo file-contents > text-file

It's OK if there's no correction:

  $ cat > dune <<EOF
  > (alias (name no_correction)
  >   (deps )
  >   (action (progn (diff? text-file text-file-corrected)))
  > )
  > EOF
  $ dune build @no_correction

Dependency on the first argument of diff? is automatically added
and dune correctly complains

  $ cat > dune <<EOF
  > (alias (name correction1)
  >   (deps)
  >   (action
  >     (progn (bash "> text-file-corrected echo corrected-contents-1")
  >     (diff? text-file text-file-corrected)))
  > )
  > EOF

  $ dune build @correction1
  File "text-file", line 1, characters 0-0:
  Error: Files _build/default/text-file and _build/default/text-file-corrected
  differ.
  [1]

Promotion works fine:

  $ dune promote
  Promoting _build/default/text-file-corrected to text-file.
  $ cat text-file
  corrected-contents-1
  $ echo file-contents > text-file

When correction is no longer produced, dune no longer complains.
(relies on stale artifact deletion, it seems)

  $ cat > dune <<EOF
  > (alias (name correction1)
  >   (deps text-file)
  >   (action
  >     (progn 
  >     (diff? text-file text-file-corrected)))
  > )
  > EOF
  $ echo new-contents > text-file
  $ dune build @correction1

Promotion should work when sandboxing is used:

  $ cat > dune <<EOF
  > (alias (name correction1)
  >   (deps)
  >   (action
  >     (progn
  >       (bash "echo another-correction > text-file-corrected")
  >       (diff? text-file text-file-corrected)))
  > )
  > EOF

  $ dune build @correction1 --sandbox copy
  File "text-file", line 1, characters 0-0:
  Error: Files
  _build/.sandbox/150b972ad59fdd3e13294c94880afcfd/default/text-file and
  _build/.sandbox/150b972ad59fdd3e13294c94880afcfd/default/text-file-corrected
  differ.
  [1]

  $ dune promote
  Skipping promotion of _build/default/text-file-corrected to text-file as the file is missing.

Dependency on the second argument of diff? is *not* automatically added.
This is fine because we think of it as an intermediate file rather than dep.

  $ cat > dune <<EOF
  > (alias (name correction1)
  >   (deps)
  >   (action
  >     (progn
  >     (diff? text-file text-file-corrected)))
  > )
  > EOF

  $ > text-file-corrected echo corrected-contents-1

  $ dune build @correction1

But dune looks at this file if it exists. This is a bit of a bug.
One fix is to not look at it, and flag its existence as an error.

  $ dune build text-file-corrected
  $ dune build @correction1

  $ rm -r _build

  $ dune build text-file-corrected
  $ dune build @correction1
  File "text-file", line 1, characters 0-0:
  Error: Files _build/default/text-file and _build/default/text-file-corrected
  differ.
  [1]

Sandboxing doesn't necessarily help either because dune thinks
[diff?] is not worth sandboxing.

  $ dune build text-file-corrected
  $ dune build @correction1 --sandbox copy
  File "text-file", line 1, characters 0-0:
  Error: Files _build/default/text-file and _build/default/text-file-corrected
  differ.
  [1]

Sandboxing does help if the command producing the
correction is non-trivial.

  $ cat > dune <<EOF
  > (alias (name correction1)
  >   (deps)
  >   (action
  >     (progn
  >       (bash "true")
  >       (diff? text-file text-file-corrected)))
  > )
  > EOF

  $ dune build @correction1 --sandbox copy

