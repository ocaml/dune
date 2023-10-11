Sandbox a rule that depends on a directory target using the copying sandbox
mode:

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (using directory-targets 0.1)
  > EOF

  $ cat >print-permissions.sh <<EOF
  > /usr/bin/env sh
  > dune_cmd ls -d output output/y output/x output/subdir output/subdir/z
  > echo ""
  > EOF

  $ chmod +x print-permissions.sh

  $ cat >dune <<EOF
  > (rule
  >  (deps print-permissions.sh)
  >  (targets (dir output))
  >  (action (system "\| echo building output/
  >                  "\| mkdir -p output/subdir
  >                  "\| touch output/x output/y output/subdir/z
  >                  "\| chmod 0777 output/
  >                  "\| chmod 0444 output/y
  >                  "\| chmod 0776 output/subdir
  >                  "\| chmod 0666 output/subdir/z
  >                  "\| . ./print-permissions.sh
  >          )))
  > (rule
  >  (target foo)
  >  (deps print-permissions.sh (sandbox always) output/)
  >  (action (system "\| . ./print-permissions.sh
  >                  "\| touch foo;
  >          )))
  > EOF

  $ dune build foo --sandbox=copy
  building output/
  drwxrwxrwx output
  -r--r--r-- output/y
  -rw-r--r-- output/x
  drwxrwxrw- output/subdir
  -rw-rw-rw- output/subdir/z
  
  drwxr-xr-x output
  -r--r--r-- output/y
  -r--r--r-- output/x
  drwxr-xr-x output/subdir
  -r--r--r-- output/subdir/z
  

  $ ( cd _build/default && ../../print-permissions.sh )
  drwxrwxrwx output
  -r--r--r-- output/y
  -r--r--r-- output/x
  drwxrwxrw- output/subdir
  -r--r--r-- output/subdir/z
  
