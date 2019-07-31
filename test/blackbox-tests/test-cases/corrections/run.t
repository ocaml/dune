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
