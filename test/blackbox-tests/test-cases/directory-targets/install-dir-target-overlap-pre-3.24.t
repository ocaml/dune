Pre-3.24 projects get a version-gate error when two `(install (dirs ...))`
entries resolve to the same destination, instead of the merge that 3.24+
allows.

  $ cat >dune-project <<EOF
  > (lang dune 3.23)
  > (using directory-targets 0.1)
  > (package (name shared_files))
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (target (dir a))
  >  (action
  >   (progn
  >    (run mkdir -p a/share)
  >    (run touch a/share/readme_a.txt))))
  > 
  > (rule
  >  (target (dir b))
  >  (action
  >   (progn
  >    (run mkdir -p b/share)
  >    (run touch b/share/readme_b.txt))))
  > 
  > (install
  >  (section share_root)
  >  (dirs
  >   (b/share as .)
  >   (a/share as .)))
  > EOF

  $ dune build @install
  File "dune", line 18, characters 3-10:
  18 |   (b/share as .)
          ^^^^^^^
  Error: Multiple (install (dirs ...)) entries resolve to the same destination
  directory. Merging is supported with (lang dune 3.24) or later; this stanza
  uses (lang dune 3.23).
  Hint: Upgrade the project's (lang dune ...) version, or split the entries
  into distinct destination directories.
  File "dune", line 19, characters 3-10:
  19 |   (a/share as .)))
          ^^^^^^^
  Error: Multiple (install (dirs ...)) entries resolve to the same destination
  directory. Merging is supported with (lang dune 3.24) or later; this stanza
  uses (lang dune 3.23).
  Hint: Upgrade the project's (lang dune ...) version, or split the entries
  into distinct destination directories.
  [1]
