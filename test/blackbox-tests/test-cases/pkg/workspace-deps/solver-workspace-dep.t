[dune pkg lock] is asked to lock a project where a mock-repository
package depends on a workspace package.

  $ mkrepo
  $ add_mock_repo_if_needed

The graph crosses the workspace boundary twice:
main (workspace) -> mylock (locked) -> util (workspace) -> leaf (locked).
The repository also contains util, but solving must use the supplied workspace
version rather than lock another definition of that package.

  $ mkpkg leaf
  $ mkpkg util 99.0
  $ mkpkg mylock <<EOF
  > depends: [ "util" {= "1.2"} ]
  > build: [ [ "sh" "-c" "test '%{util:version}%' = 1.2" ] ]
  > EOF

  $ make_dune_project 3.24
  $ cat >> dune-project <<EOF
  > (package (name main) (depends mylock) (allow_empty))
  > (package (name util) (version 1.2) (depends leaf) (allow_empty))
  > EOF

Locking fills only the missing packages. Workspace util satisfies mylock's
version constraint and does not get a lock entry.

  $ dune pkg lock
  Solution for dune.lock
  
  Dependencies common to all supported platforms:
  - leaf.0.0.1
  - mylock.0.0.1
  $ ls dune.lock/*.pkg
  dune.lock/leaf.0.0.1.pkg
  dune.lock/mylock.0.0.1.pkg
  $ dune pkg validate-lockdir
  $ dune describe pkg list-locked-dependencies --transitive
  Dependencies of local packages locked in dune.lock
  - Transitive dependencies of local package main.dev
    - leaf.0.0.1
    - mylock.0.0.1
    - util.1.2
    
  - Transitive dependencies of local package util.1.2
    - leaf.0.0.1
    

The generated lockdir can be built, and the recipe reads the supplied workspace
version rather than the repository's unrelated util version.

  $ cat > dune <<'EOF'
  > (rule
  >  (target result)
  >  (deps (package mylock))
  >  (action (write-file %{target} done)))
  > EOF
  $ dune build result
  $ cat _build/default/result
  done

Changing a supplied workspace version can invalidate a repository constraint.
Currently validation misses that change, even though a fresh solve rejects it.

  $ sed 's/(version 1.2)/(version 2.0)/' dune-project > dune-project.new
  $ mv dune-project.new dune-project
  $ dune pkg validate-lockdir
  $ dune pkg lock >solve-error 2>&1
  [1]
  $ grep '^Error:' solve-error
  Error:
  $ grep -q '1.2' solve-error
  $ grep -q '2.0' solve-error
