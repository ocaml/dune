When two `(install (dirs ...))` entries merge but contribute the same
leaf path, the build fails. The engine catches this at rule registration
(two rules with identical targets).

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name p))
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (target (dir a))
  >  (action
  >   (progn
  >    (run mkdir -p a/share)
  >    (run touch a/share/conflict.txt))))
  > 
  > (rule
  >  (target (dir b))
  >  (action
  >   (progn
  >    (run mkdir -p b/share)
  >    (run touch b/share/conflict.txt))))
  > 
  > (install
  >  (section share_root)
  >  (dirs
  >   (b/share as .)
  >   (a/share as .)))
  > EOF

  $ dune build @install
  Error: Multiple rules generated for
  _build/install/default/share/conflict.txt:
  - dune:18
  - dune:19
  -> required by _build/default/p.install
  -> required by alias install
  [1]
