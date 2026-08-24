Recover from interruption after the canonical lock directory was moved to its
hidden staging path.

Start with a generated, validated lock directory and reproduce the filesystem
state persisted at the interruption point.

  $ mkrepo
  $ mkpkg a
  $ mkpkg b
  $ add_mock_repo_if_needed
  $ make_project a > dune-project
  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  - a.0.0.1
  $ dune pkg validate-lockdir
  $ mv ${default_lock_dir} .${default_lock_dir}
  $ if [ -e ${default_lock_dir} ]; then echo "dune.lock present"; else echo "dune.lock absent"; fi
  dune.lock absent
  $ if [ -d .${default_lock_dir} ]; then echo ".dune.lock present"; else echo ".dune.lock absent"; fi
  .dune.lock present

Rerunning for a different solution must install the newly requested canonical
lock directory rather than silently restoring the stale backup.

  $ make_project b > dune-project
  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  - b.0.0.1
  $ if [ -d ${default_lock_dir} ]; then echo "dune.lock present"; else echo "dune.lock absent"; fi
  dune.lock present
  $ cat ${default_lock_dir}/b.0.0.1.pkg
  (version 0.0.1)
  $ if [ -e ${default_lock_dir}/a.0.0.1.pkg ]; then echo "stale solution restored"; else echo "requested solution installed"; fi
  requested solution installed
  $ dune pkg validate-lockdir
  $ if [ -e .${default_lock_dir} ]; then echo ".dune.lock present"; else echo ".dune.lock absent"; fi
  .dune.lock absent
