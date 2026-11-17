Packages can export environment variables.

Inspect the final rule, not just rebuilds: digest-qualified paths currently
mask missing environment dependencies. These checks should report true.

  $ make_lockdir
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (exported_env
  >  (= FOO bar)
  >  (= BAR xxx)
  >  (+= BAR yyy)
  >  (:= BAR zzz))
  > EOF

  $ make_lockpkg usetest <<'EOF'
  > (depends test)
  > (version 1.2.3)
  > (build
  >  (progn
  >   (system "\| echo FOO=$FOO
  >           "\| echo BAR=$BAR
  >           "\| echo OPAM_PACKAGE_NAME=$OPAM_PACKAGE_NAME
  >           "\| echo OPAM_PACKAGE_VERSION=$OPAM_PACKAGE_VERSION
  >           "\| echo OPAMSWITCH=$OPAMSWITCH
  >   )
  >   (run mkdir -p %{prefix})
  >   (system "echo $FOO/$OPAM_PACKAGE_VERSION > %{prefix}/environment")))
  > EOF

  $ build_pkg usetest
  FOO=bar
  BAR=zzz:yyy:xxx
  OPAM_PACKAGE_NAME=usetest
  OPAM_PACKAGE_VERSION=1.2.3
  OPAMSWITCH=dune
  $ cat "$(get_build_pkg_dir usetest)/target/environment"
  bar/1.2.3
  $ dune rules --format=json "$(get_build_pkg_dir usetest)/target" \
  >   | jq '.[] | .deps | index({Env: "FOO"}) != null'
  false

Changing only a dependency's exported environment must rebuild its consumer.

  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (exported_env
  >  (= FOO baz)
  >  (= BAR xxx)
  >  (+= BAR yyy)
  >  (:= BAR zzz))
  > EOF
  $ build_pkg usetest > /dev/null 2>&1
  $ cat "$(get_build_pkg_dir usetest)/target/environment"
  baz/1.2.3
  $ dune rules --format=json "$(get_build_pkg_dir usetest)/target" \
  >   | jq '.[] | .deps | index({Env: "FOO"}) != null'
  false

Changing the package version also invalidates the build.

  $ dune_cmd subst '1[.]2[.]3' '1.2.4' < dune.lock/usetest.pkg > updated.pkg
  $ mv updated.pkg dune.lock/usetest.pkg
  $ build_pkg usetest > /dev/null 2>&1
  $ cat "$(get_build_pkg_dir usetest)/target/environment"
  baz/1.2.4

Workspace rules receive exports both through explicit package dependencies
and through their directory environment. Both must record the environment
dependency even when package artifact paths change after a lock edit.

  $ make_dune_project 3.24
  $ cat >dune <<'EOF'
  > (rule
  >  (target workspace-output)
  >  (deps (package test))
  >  (action
  >   (with-stdout-to %{target} (system "echo $FOO"))))
  > (rule
  >  (target implicit-output)
  >  (action
  >   (with-stdout-to %{target} (system "echo $FOO"))))
  > (rule
  >  (target declared-env-output)
  >  (deps (env_var FOO))
  >  (action
  >   (with-stdout-to %{target} (system "echo $FOO"))))
  > EOF
  $ dune build workspace-output implicit-output declared-env-output
  $ cat _build/default/workspace-output _build/default/implicit-output
  baz
  baz
  $ dune rules --format=json workspace-output \
  >   | jq '.[] | .deps | index({Env: "FOO"}) != null'
  false
  $ dune rules --format=json implicit-output \
  >   | jq '.[] | .deps | index({Env: "FOO"}) != null'
  false

An explicit declaration distinguishes a present environment dependency
from an absent one.

  $ dune rules --format=json declared-env-output |
  >   jq -c '.[] | .deps | { declared: (index({Env: "FOO"}) != null),
  >                         absent: (index({Env: "MISSING"}) != null) }'
  {"declared":true,"absent":false}
