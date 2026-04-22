
function init_bootstrap() {
  mkdir boot
  cp ../../../../boot/types.ml boot/types.ml
  cp ../../../../boot/duneboot.ml boot/duneboot.ml
  cp ../../../../boot/pps.mll boot/pps.mll
  cp ../../../../boot/pps.mli boot/pps.mli

  cat > dune-project <<EOF
(lang dune 3.21)
(using dune-bootstrap-info 0.1)
EOF

  # These libraries are hardcoded and expected to exist:
  mkdir -p vendor/csexp/src
  mkdir -p vendor/pp/src
  mkdir -p vendor/re/src
  mkdir -p vendor/spawn/src
  mkdir -p vendor/uutf

  mkdir bin

  ocamlopt="$(which ocamlopt.opt)"
  export BUILD_PATH_PREFIX_MAP="/OCAMLOPT=$ocamlopt:$BUILD_PATH_PREFIX_MAP"
  ocamlc="$(which ocamlc.opt)"
  export BUILD_PATH_PREFIX_MAP="/OCAMLC=$ocamlc:$BUILD_PATH_PREFIX_MAP"
}

function make_module() {
  mkdir -p "$(dirname "$1")"
  cat >"$1" <<- EOF
module Spawn = Spawn
module Re = Re
module Csexp = Csexp
module Pp = Pp
module Uutf = Uutf
EOF
}

function normalize_bootstrap_output() {
  # bootstrap.exe runs an OCaml script from a temp file with a random path.
  # shellcheck disable=SC2016
  dune_cmd subst '[^ ]*[/\\]\.duneboot[0-9a-f]+main' '$DUNEBOOT'
}

# Creates a fake dune executable that depends on [$1] and emits bootstrap info
# which is then used to bootstrap the fake dune.
function create_dune() {
  local output
  local bootstrap_status=0

  cat > bin/main.ml
  cat > bin/dune <<- EOF
	(executable
	 (name main)
	 (libraries $1)
	 (bootstrap_info bootstrap-info))
	EOF
  dune build bin/bootstrap-info
  cp _build/default/bin/bootstrap-info boot/libs.ml
  output="$(mktemp)"
  bootstrap.exe >"$output" 2>&1 || bootstrap_status=$?
  normalize_bootstrap_output <"$output"
  rm -f "$output"
  [ "$bootstrap_status" -eq 0 ] || return "$bootstrap_status"
  _boot/dune.exe
}
