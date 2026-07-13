Explicit sandbox selections retain their normal filesystem semantics.

  $ make_dune_project 3.23

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

Explicit symlink and hardlink selections use the canonical digest path and
give dependencies the selected link semantics.

  $ for mode in symlink hardlink; do
  >   dune shell --sandbox="$mode" _build/default/sub/out -- sh -c '
  >     selected=$(cat "$DUNE_SHELL/sandbox-mode")
  >     echo "$selected"
  >     printf "canonical-path: "; echo "$PWD"
  >     case "$selected" in
  >       symlink)
  >         test -L prepared-input && echo "symlink-semantics: linked" ;;
  >       hardlink)
  >         links=$(dune_cmd stat hardlinks prepared-input)
  >         test "$links" -gt 1 && echo "hardlink-semantics: shared" ;;
  >     esac
  >   ' | censor >"$mode.stdout"
  >   printf "%s-mode: " "$mode"; cat "$mode.stdout"
  > done
  symlink-mode: symlink
  canonical-path: $PWD/_build/.sandbox/$DIGEST/default/sub
  symlink-semantics: linked
  hardlink-mode: hardlink
  canonical-path: $PWD/_build/.sandbox/$DIGEST/default/sub
  hardlink-semantics: shared
