export XDG_CACHE_HOME="$PWD/.cache"

init_project() {
  cat > "dune-project" <<- EOF
	(lang dune 3.20)
	(using oxcaml 0.1)
	EOF
}

make_dir_with_dune() {
  path="$1"
  mkdir -p $path
  cat > "$path/dune"
}

make_dummy_intf() {
  dir="$1"
  name="$2"
  cat >> "$dir/$name.mli" <<- EOF
	type t
	val f : t -> unit
	EOF
}

make_dummy_impl() {
  dir="$1"
  name="$2"
  cat >> "$dir/$name.ml" <<- EOF
	type t = int
	let f _ = ()
	EOF
}

make_lib_impl() {
  name="$1"
  implements="$2"
  make_dir_with_dune $name <<- EOF
	(library
	  (name $name)
	  (implements $implements))
	EOF
}

target_cmi() {
  echo "./$1/.$1.objs/byte/$1.cmi"
}

build_target_cmi() {
  echo "./_build/default/$1/.$1.objs/byte/$1.cmi"
}
