Explicit sandbox selections retain their normal filesystem semantics, and
command mode returns the user's exit status directly.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.23)
  > EOF

  $ mkdir sub
  $ cat > sub/dune <<'EOF'
  > (rule
  >  (target prepared-input)
  >  (deps source.txt)
  >  (action (copy source.txt prepared-input)))
  > 
  > (rule
  >  (target out)
  >  (deps prepared-input)
  >  (action (with-stdout-to out (cat prepared-input))))
  > EOF
  $ echo prepared > sub/source.txt
  $ export ROOT=$PWD

Explicit symlink and hardlink selections use the canonical digest path and
give dependencies the selected link semantics.

  $ for mode in symlink hardlink; do
  >   dune shell --sandbox="$mode" _build/default/sub/out -- sh -c '
  >     selected=$(cat "$DUNE_SHELL/sandbox-mode")
  >     digest=$(cat "$DUNE_SHELL/rule-digest")
  >     echo "$selected"
  >     if test "$PWD" = "$ROOT/_build/.sandbox/$digest/default/sub"; then
  >       echo "canonical-path: exact"
  >     else
  >       echo "canonical-path: different"
  >     fi
  >     case "$selected" in
  >       symlink)
  >         test -L prepared-input && echo "symlink-semantics: linked" ;;
  >       hardlink)
  >         links=$(dune_cmd stat hardlinks prepared-input)
  >         test "$links" -gt 1 && echo "hardlink-semantics: shared" ;;
  >     esac
  >   ' >"$mode.stdout" 2>"$mode.stderr"
  >   printf "%s-mode: " "$mode"; cat "$mode.stdout"
  > done
  symlink-mode: symlink
  canonical-path: exact
  symlink-semantics: linked
  hardlink-mode: hardlink
  canonical-path: exact
  hardlink-semantics: shared

The non-interactive command's status is returned directly without a wrapped
Dune process error. Its sandbox and metadata are still cleaned up.

  $ dune shell --sandbox=copy _build/default/sub/out -- sh -c '
  > printf "%s\n" "$PWD" > "$ROOT/nonzero-sandbox"
  > printf "%s\n" "$DUNE_SHELL" > "$ROOT/nonzero-metadata"
  > exit 7
  > ' \
  >   >exit.stdout 2>exit.stderr
  [7]
  $ grep '^Error:' exit.stderr || echo "no wrapped error"
  no wrapped error
  $ test ! -e "$(cat nonzero-sandbox)" && echo "nonzero-sandbox: cleaned"
  nonzero-sandbox: cleaned
  $ test ! -e "$(cat nonzero-metadata)" && echo "nonzero-metadata: cleaned"
  nonzero-metadata: cleaned
