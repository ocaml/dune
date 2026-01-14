Installing multiple directories into share_root currently crashes dune

Create two packages
  $ cat >dune-project <<EOF
  > (lang dune 3.20)
  > (using directory-targets 0.1)
  > (package (name a))
  > (package (name b))
  > EOF

Create two rules that create directories, and trys to install them both

  $ cat >dune <<EOF
  > (rule
  >  (target
  >   (dir a))
  >  (action
  >   (progn
  >    (run mkdir -p a/share)
  >    (run touch a/share/readme_a.txt))))
  > 
  > (install
  >  (section share_root)
  >  (package a)
  >  (dirs
  >   (a/share as .)))
  > 
  > (rule
  >  (target
  >   (dir b))
  >  (action
  >   (progn
  >    (run mkdir -p b/share)
  >    (run touch b/share/readme_b.txt))))
  > 
  > (install
  >  (section share_root)
  >  (package b)
  >  (dirs
  >   (b/share as .)))
  > EOF


When only one package is installed, result is as expected
  $ dune build @install --only a
  $ ls -RL _build/install/default
  _build/install/default:
  lib
  share
  
  _build/install/default/lib:
  a
  
  _build/install/default/lib/a:
  META
  dune-package
  
  _build/install/default/share:
  readme_a.txt

Same with the other, though note that this removes the previous
  $ dune build @install --only b
  $ ls -RL _build/install/default
  _build/install/default:
  lib
  share
  
  _build/install/default/lib:
  b
  
  _build/install/default/lib/b:
  META
  dune-package
  
  _build/install/default/share:
  readme_b.txt

  $ dune clean

Both at the same time leads to a crash
  $ dune build @install 2>&1 | grep "must not crash"
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  [1]
  $ ls -RL _build/install/default
  ls: cannot access '_build/install/default': No such file or directory
  [2]
