Test the %{git-sha} variable.

Without a git repository, %{git-sha} expands to the empty string.

  $ make_dune_project 3.24
  $ cat > dune << EOF
  > (rule
  >  (target sha.txt)
  >  (action (with-stdout-to sha.txt (echo "%{git-sha}"))))
  > EOF
  $ dune build sha.txt
  $ cat _build/default/sha.txt

In a git repository with a commit, %{git-sha} expands to the short git SHA.

  $ git init --quiet
  $ git config user.email "test@test.com"
  $ git config user.name "Test"
  $ git add .
  $ git commit -q -m "initial"
  $ rm -f _build/default/sha.txt
  $ dune build sha.txt
  $ [ "$(cat _build/default/sha.txt)" = "$(git rev-parse --short HEAD)" ] && echo "sha matches"
  sha matches

In a git repository with no commits, %{git-sha} expands to the empty string.

  $ rm -rf .git
  $ git init --quiet
  $ rm -f _build/default/sha.txt
  $ dune build sha.txt
  $ cat _build/default/sha.txt

Version check: %{git-sha} requires dune >= 3.24.

  $ make_dune_project 3.23
  $ dune build sha.txt
  File "dune", line 3, characters 40-50:
  3 |  (action (with-stdout-to sha.txt (echo "%{git-sha}"))))
                                              ^^^^^^^^^^
  Error: %{git-sha} is only available since version 3.24 of the dune language.
  Please update your dune-project file to have (lang dune 3.24).
  [1]
