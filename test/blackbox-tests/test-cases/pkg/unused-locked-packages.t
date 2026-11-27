Locked packages that nothing asks for are neither fetched nor loaded, so they
cannot fail an unrelated build.

  $ make_lockdir
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name use-unfetchable) (dir use-unfetchable) (allow_empty)
  >  (depends unfetchable))
  > (package (name use-broken) (dir use-broken) (allow_empty)
  >  (depends broken))
  > EOF
  $ echo '(rule (with-stdout-to x (echo built)))' > dune

A package that builds, one whose source does not exist, and one whose Dune
project does not load.

  $ mkdir _good _broken
  $ (cd _good && make_dune_project_with_package 3.25 good)
  $ echo '(library (public_name good))' > _good/dune
  $ make_dune_source_lockpkg native good "$PWD/_good"
  $ make_lockpkg unfetchable <<EOF
  > (version 1.0)
  > (source (copy $PWD/_absent))
  > (build (run false))
  > EOF
  $ (cd _broken && make_dune_project_with_package 3.25 broken)
  $ echo '(library (public_name broken) (unknown_field))' > _broken/dune
  $ make_dune_source_lockpkg native broken "$PWD/_broken"

The unrelated target builds and no package is fetched.

  $ dune build ./x
  $ cat _build/default/x
  built
  $ test ! -d _build/_fetch

Asking for the unfetchable package fails. The error should say what asked for
the package; currently there is no "required by" chain.

  $ mkdir use-unfetchable use-broken
  $ cat > use-unfetchable/dune <<'EOF'
  > (rule
  >  (alias demand)
  >  (deps (package unfetchable))
  >  (action (echo unreachable)))
  > EOF
  $ dune build @use-unfetchable/demand 2>&1 | dune_cmd subst "$PWD" PWD \
  > | dune_cmd delete ' *\^\^*$' | sed 's/characters [0-9-]*/characters X/'
  File "_build/_private/default/.lock/dune.lock/unfetchable.pkg", line 2, characters X:
  2 | (source (copy PWD/_absent))
  Error:
  PWD/_absent
  does not exist
  [1]

Installing a workspace library asks for its own directory of the install
tree. A locked package that cannot be fetched is not asked for.

  $ cat >> dune-project <<'EOF'
  > (package (name ws) (dir ws))
  > EOF
  $ mkdir ws
  $ echo '(library (public_name ws))' > ws/dune
  $ make_lockpkg remote <<EOF
  > (version 1.0)
  > (source (fetch (url "http://0.0.0.0:1")))
  > EOF
  $ dune build _build/install/default/lib/ws/META
  $ test ! -d _build/_fetch
