Package resolution creating a cycle

  $ . ./helpers.sh

  $ make_lockdir

  $ cat > ${default_lock_dir}/a.pkg <<EOF
  > (version 0.0.1)
  > (depends b)
  > EOF
  $ cat > ${default_lock_dir}/b.pkg <<EOF
  > (version 0.0.1)
  > (depends c)
  > EOF
  $ cat > ${default_lock_dir}/c.pkg <<EOF
  > (version 0.0.1)
  > (depends a)
  > EOF

  $ build_pkg a
  Error: Dependency cycle between:
     - package a
  -> - package c
  -> - package b
  -> - package a
  [1]
