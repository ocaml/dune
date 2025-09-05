export XDG_CACHE_HOME="$PWD/.cache"

init_project() {
  cat > "dune-project" <<EOF
(lang dune 3.20)
(using oxcaml 0.1)
EOF
}

make_dir_with_dune() {
  path="$1"
  mkdir -p $path
  cat > "$path/dune"
}

target_cmi() {
  echo "./$1/.$1.objs/byte/$1.cmi"
}

build_target_cmi() {
  echo "./_build/default/$1/.$1.objs/byte/$1.cmi"
}
