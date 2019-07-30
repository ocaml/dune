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

# CR-someday aalekseyev:
# Weird: if dependency is missing, dune fails to report a correction.
# This is an idiom used in the wild,
# and in fact the docs suggest that one should do something like this,
# and you can't even specify optional targets at all.

  $ cat > dune <<EOF
  > (alias (name correction1)
  >   (deps)
  >   (action
  >     (progn (bash "> text-file-corrected echo corrected-contents-1")
  >     (diff? text-file text-file-corrected)))
  > )
  > EOF

  $ dune build @correction1

  $ dune build text-file
  $ dune build @correction1

The correction shines through once the source file is created in the build dir:

  $ rm -r _build
  $ dune build text-file
  $ dune build @correction1
  File "text-file", line 1, characters 0-0:
  Error: Files _build/default/text-file and _build/default/text-file-corrected
  differ.
  [1]

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
