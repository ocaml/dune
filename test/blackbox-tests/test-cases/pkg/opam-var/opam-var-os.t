  $ . ../helpers.sh

Here we test global opam variables that are system specific. Since these values change
between systems, we can't hardcode them in the test. Instead, we use the opam var command
to compare their values.

# arch os os-distribution os-family os-version user group


  $ mkrepo
  > mkpkg testpkg <<EOF
  > build: [
  >   ["echo" arch]
  >   ["echo" os]
  >   ["echo" os-distribution]
  >   ["echo" os-family]
  >   ["echo" os-version]
  > ]
  > EOF
  > solve testpkg
  Solution for dune.lock:
  - testpkg.0.0.1
  $ cat dune.lock/testpkg.pkg 
  (version 0.0.1)
  
  (build
   (progn
    (run echo %{arch})
    (run echo %{os})
    (run echo %{os_distribution})
    (run echo %{os_family})
    (run echo %{os_version})))

We write all the dune values to a file and then diff them with the output of opam var.

  $ build_pkg testpkg 2> dune-vars

The two files should be identical.

  $ diff --label="opam-vars"  opam-vars --label="dune-vars" dune-vars 
