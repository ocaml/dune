
mkdir boot
cp ../../../../boot/types.ml boot/types.ml
cp ../../../../boot/duneboot.ml boot/duneboot.ml

cat > dune-project <<EOF
(lang dune 3.21)
(using dune-bootstrap-info 0.1)
EOF

# These libraries are hardcoded and expected to exist:
mkdir -p vendor/re/src
mkdir -p vendor/spawn/src
mkdir -p vendor/uutf

touch vendor/re/src/re.ml
touch vendor/spawn/src/spawn.ml
touch vendor/uutf/uutf.ml

mkdir bin

# Creates a fake dune executable that depends on [$1] and emits bootstrap info
# which is then used to bootstrap the fake dune.
create_dune() {
  cat > bin/main.ml
  cat > bin/dune <<EOF
(executable
 (name main)
 (libraries $1)
 (bootstrap_info bootstrap-info))
EOF
  dune build bin/bootstrap-info
  cp _build/default/bin/bootstrap-info boot/libs.ml
  bootstrap.exe && _boot/dune.exe
}

