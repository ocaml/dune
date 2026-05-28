Test that regular file targets can be restored from another workspace using the
same shared cache in copy mode.

  $ export XDG_CACHE_HOME=$PWD/.xdg-cache
  $ setup_xdg_runtime_dir
  $ export DUNE_TRACE=cache

  $ make_workspace () {
  >   dir=$1
  >   mkdir "$dir"
  >   cat > "$dir/dune-project" <<EOF
  > (lang dune 3.23)
  > EOF
  >   cat > "$dir/dune" <<EOF
  > (rule
  >  (target out)
  >  (action (bash "echo ran; echo shared > out")))
  > EOF
  > }
  $ make_workspace a
  $ make_workspace b

  $ (cd a && dune build --cache=enabled --cache-storage-mode=copy out)
  ran

  $ (cd b && dune build --cache=enabled --cache-storage-mode=copy out)
  $ cat b/_build/default/out
  shared
  $ (cd b && dune trace cat | jq 'select(.cat == "cache" and .name == "hit" and .args.head == "_build/default/out") | { name, target: .args.head }')
  {
    "name": "hit",
    "target": "_build/default/out"
  }
