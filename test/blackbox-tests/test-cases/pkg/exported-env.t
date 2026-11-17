Packages can export environment variables.

Inspect the final rule, not just rebuilds: digest-qualified paths currently
mask missing environment dependencies. These checks should report true.

  $ export UNRELATED_ENV=present
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
  >                         absent: (index({Env: "MISSING"}) != null),
  >                         ambient: (index({Env: "UNRELATED_ENV"}) != null) }'
  {"declared":true,"absent":false,"ambient":false}

Host and target contexts can supply package exports from different lockdirs.
Use a separate project with an empty package: no binary lookup is involved.

  $ mkdir cross-context
  $ cd cross-context
  $ make_dune_project 3.25
  $ cat >>dune-project <<'EOF'
  > (package (name consumer) (allow_empty) (dir .) (depends provider))
  > EOF
  $ make_lockdir
  $ make_lockpkg provider <<'EOF'
  > (version 0.0.1)
  > EOF
  $ cp -R dune.lock host.lock
  $ cp -R dune.lock target.lock
  $ cat >>host.lock/provider.pkg <<'EOF'
  > (exported_env
  >  (= HOST_ONLY host)
  >  (+= PATH host-export))
  > EOF
  $ cat >>target.lock/provider.pkg <<'EOF'
  > (exported_env (= TARGET_ONLY target))
  > EOF
  $ cat >dune-workspace <<'EOF'
  > (lang dune 3.25)
  > (pkg enabled)
  > (lock_dir (path dune.lock))
  > (lock_dir (path host.lock))
  > (lock_dir (path target.lock))
  > (context (default))
  > (context (default (name host) (lock_dir host.lock)))
  > (context (default (name target) (host host) (lock_dir target.lock)))
  > EOF
  $ cat >dune <<'EOF'
  > (rule
  >  (enabled_if (= %{context_name} target))
  >  (target exports)
  >  (deps (package provider))
  >  (action
  >   (with-stdout-to %{target}
  >    (system "\| echo $HOST_ONLY/$TARGET_ONLY
  >            "\| case $PATH in
  >            "\|   *host-export*) echo host-path ;;
  >            "\|   *) echo missing-host-path ;;
  >            "\| esac
  >    ))))
  > EOF
  $ dune build _build/target/exports
  $ cat _build/target/exports
  host/target
  host-path

All three explicit exports should be tracked by the final rule, regardless of
which context supplies them. Automatic installation paths are not the export
being checked here: PATH is explicitly exported by the host package.

  $ dune rules --format=json _build/target/exports |
  >   jq -c '.[] | .deps | { host: (index({Env: "HOST_ONLY"}) != null),
  >                         target: (index({Env: "TARGET_ONLY"}) != null),
  >                         path: (index({Env: "PATH"}) != null) }'
  {"host":false,"target":false,"path":false}

A later export can clear a merged binding on Windows. Explicit package
dependencies still supply their selected exports, and the directory's narrowed
PATH must also be tracked.

  $ cat >host.lock/suppressor.pkg <<'EOF'
  > (version 0.0.1)
  > (depends provider)
  > (exported_env (= HOST_ONLY "") (= PATH ""))
  > EOF
  $ cat >target.lock/suppressor.pkg <<'EOF'
  > (version 0.0.1)
  > (depends provider)
  > (exported_env (= PATH ""))
  > EOF
  $ cat >>dune <<'EOF'
  > (rule
  >  (enabled_if (= %{context_name} target))
  >  (target implicit-path)
  >  (action
  >   (with-stdout-to %{target}
  >    (system "case $PATH in *host-export*) echo host-path ;; esac"))))
  > EOF
  $ dune build _build/target/exports _build/target/implicit-path
  $ cat _build/target/exports _build/target/implicit-path
  host/target
  host-path
  host-path
  $ dune rules --format=json _build/target/exports |
  >   jq -c '.[] | .deps | { host: (index({Env: "HOST_ONLY"}) != null),
  >                         path: (index({Env: "PATH"}) != null) }'
  {"host":false,"path":false}
  $ dune rules --format=json _build/target/implicit-path |
  >   jq '.[] | .deps | index({Env: "PATH"}) != null'
  false
