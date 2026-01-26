# Technically, this test should depend on something that comes from the OS so
# that it re-runs after an update. That's a little hard to express however, and
# one must remember to manually force to re-run it after an upgrade.

Here we test global opam variables that are system specific. Since these values change
between systems, we can't hardcode them in the test. Instead, we use the opam var command
to compare their values.

  $ export OPAMROOT=$(mktemp -d)
  $ mkdir -p $OPAMROOT/default/.opam-switch/
  $ cat >$OPAMROOT/default/.opam-switch/switch-config <<EOF
  > opam-version: "2.0"
  > EOF
  $ cat >$OPAMROOT/config <<EOF
  > opam-version: "2.0"
  > switch: "default"
  > installed-switches: ["default"]
  > EOF
  $ v() { opam --cli 2.1 var $1; }
  $ { v arch ; v os ; v os-distribution ; v os-family ; v os-version ; } > opam-vars

# arch os os-distribution os-family os-version user group

These variables are usually set to keep tests consistent across different
platforms but for this test we need to expose the real platform to dune, so
unset them all.
  $ unset DUNE_CONFIG__OS DUNE_CONFIG__ARCH DUNE_CONFIG__OS_FAMILY DUNE_CONFIG__OS_DISTRIBUTION DUNE_CONFIG__OS_VERSION DUNE_CONFIG__SYS_OCAML_VERSION

  $ mkrepo
  $ mkpkg testpkg <<EOF
  > build: [
  >   ["echo" arch]
  >   ["echo" os]
  >   ["echo" os-distribution]
  >   ["echo" os-family]
  >   ["echo" os-version]
  > ]
  > EOF
  $ solve testpkg
  Solution for dune.lock:
  - testpkg.0.0.1
  $ cat ${default_lock_dir}/testpkg.0.0.1.pkg
  (version 0.0.1)
  
  (build
   (all_platforms
    ((action
      (progn
       (run echo %{arch})
       (run echo %{os})
       (run echo %{os_distribution})
       (run echo %{os_family})
       (run echo %{os_version}))))))

We write all the dune values to a file and then diff them with the output of opam var.

  $ build_pkg testpkg 2> dune-vars

The two files should be identical.

  $ diff --label="opam-vars"  opam-vars --label="dune-vars" dune-vars

Getting the sys-ocaml-version variable consistent is rather annoying because it
depends on whether we have a system installed OCaml. So we just test it
separately here:

  $ mkpkg testpkg <<EOF
  > build: [
  >   ["echo" sys-ocaml-version]
  > ]
  > EOF
  $ solve testpkg
  Solution for dune.lock:
  - testpkg.0.0.1

  $ ocaml_version="$(ocaml -vnum)"
  $ build_pkg testpkg 2>&1 | dune_cmd subst "$ocaml_version" 'OCAML_VERSION'
  OCAML_VERSION
