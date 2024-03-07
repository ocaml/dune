is_linked() {
  nlinks=$(dune_cmd stat hardlinks "$1")
  [ "$nlinks" -gt 1 ] && echo linked || echo not linked
}
