jq() {
  command jq -L"$INSIDE_DUNE"/test/blackbox-tests "$@"
}

is_linked() {
  nlinks=$(dune_cmd stat hardlinks "$1")
  [ "$nlinks" -gt 1 ] && echo linked || echo not linked
}

export XDG_CACHE_HOME="$PWD/.cache"

init_oxcaml_project() {
  cat > "dune-project" <<- EOF
	(lang dune 3.20)
	(using oxcaml 0.1)
	EOF
}

make_dir_with_dune() {
  path="$1"
  mkdir -p "$path"
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
  make_dir_with_dune "$name" <<- EOF
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

# Git related helper scripts

# These variables are used by Git and set here so the name and email of the
# committer stay the same between different runs
export GIT_AUTHOR_NAME="Test Name"
export GIT_AUTHOR_EMAIL="test@example.com"
export GIT_COMMITTER_NAME="${GIT_AUTHOR_NAME}"
export GIT_COMMITTER_EMAIL="${GIT_AUTHOR_EMAIL}"

export DUNE_RUNNING=0

start_dune () {
    ( (dune build "$@" --passive-watch-mode > .#dune-output 2>&1) || (echo exit $? >> .#dune-output) ) &
    DUNE_PID=$!;
    export DUNE_RUNNING=1;
}

timeout="$(command -v timeout || echo gtimeout)"

with_timeout () {
    $timeout 2 "$@"
    exit_code=$?
    if [ "$exit_code" = 124 ]
    then
        echo Timed out
        cat .#dune-output
    else
        return "$exit_code"
    fi
}

stop_dune () {
    with_timeout dune shutdown;
    # On Linux, we may run into a bash pid aliasing bug that causes wait to
    # reject the pid. Therefore we use tail to wait instead.
    if [ "$(uname -s)" = "Linux" ]
    then
        # wait for all child processes
        tail --pid="$DUNE_PID" -f /dev/null;
    else
        # wait for dune to exit
        wait "$DUNE_PID";
    fi
    cat .#dune-output;
}

build () {
    with_timeout dune rpc build --wait "$@"
}
