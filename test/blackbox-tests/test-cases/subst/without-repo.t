Demonstrate $ dune subst without a git repository

  $ cat > dune-project << EOF
  > (lang dune 3.17)
  > (name test)
  > EOF

  $ echo "%%NAME%%" > README.md

  $ dune subst

  $ cat README.md
  test

dune subst should take the project version from the dune-project file when
there is no vcs

  $ cat > dune-project << EOF
  > (lang dune 3.19)
  > (name test)
  > (version 0.1)
  > EOF

  $ echo "%%VERSION%%" > README.md

  $ dune subst

  $ cat README.md
  0.1

Malformed and unknown watermarks are left alone. A run of percent signs or an
unknown watermark must not hide a known watermark immediately following it.

  $ cat > watermarks <<'EOF'
  > %NAME%%
  > %%NAME%
  > %%Name%%
  > %%UNKNOWN%%
  > %%THIS_NAME_IS_FAR_TOO_LONG%%
  > %%%NAME%%%
  > %%UNKNOWN%%NAME%%
  > EOF
  $ dune subst
  $ cat watermarks
  %NAME%%
  %%NAME%
  %%Name%%
  %%UNKNOWN%%
  %%THIS_NAME_IS_FAR_TOO_LONG%%
  %test%
  %%UNKNOWNtest

An incomplete watermark at EOF must survive unchanged in every scanner state.

  $ for text in '%' '%%' '%%NAME' '%%NAME%'; do
  >   printf '%s' "$text" > tail
  >   cp tail expected
  >   dune subst
  >   cmp tail expected
  > done

A known watermark without metadata reports its location even after a newline.

  $ cat > missing.txt <<'EOF'
  > First line
  > prefix %%PKG_LICENSE%% suffix
  > EOF
  $ dune subst
  File "missing.txt", line 2, characters 7-22:
  2 | prefix %%PKG_LICENSE%% suffix
             ^^^^^^^^^^^^^^^
  Error: variable "license" not found in dune-project file
  [1]
