`--action-runner` should not leak its control socket to spawned actions.

  $ make_dune_project 3.23
  $ cat > dune <<'EOF'
  > (rule
  >  (target fds)
  >  (action
  >   (bash
  >    "for fd in /proc/self/fd/*; do
  >       target=$(readlink \"$fd\" 2>/dev/null || true)
  >       case \"$target\" in
  >         socket:*) echo \"$fd -> $target\" ;;
  >       esac
  >     done > %{target}")))
  > EOF

  $ dune build --action-runner fds
  $ cat _build/default/fds
