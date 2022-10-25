Test that digests computed from pure dune actions are portable.

  $ export XDG_RUNTIME_DIR=$PWD/.xdg-runtime
  $ export XDG_CACHE_HOME=$PWD/.xdg-cache

  $ config=.config
  $ cat > $config <<EOF
  > (lang dune 3.5)
  > (cache enabled)
  > (cache-storage-mode copy)
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.5)
  > EOF
  $ cat > dune <<EOF
  > (rule (with-stdout-to t1 (echo foo)))
  > (rule (with-stdout-to t2 (cat t1)))
  > (rule
  >  (progn
  >   (with-stdout-to t3 (cat t1))
  >   (with-stdout-to t4 (cat t2 t2))))
  > (rule
  >  (progn
  >   (no-infer (with-stdout-to beacon (echo "")))
  >   (with-stdout-to t5 (echo baz))))
  > (rule
  >  (deps source)
  >  (targets t6 t7)
  >  (action
  >   (progn
  >    (no-infer (with-stdout-to beacon (echo "")))
  >    (with-stdout-to t6 (cat source))
  >    (with-stdout-to t7 (cat source source)))))
  > EOF
  $ cat > source <<EOF
  > \_o< COIN
  > EOF

  $ dune build t1 t2 t3 t4 t5 t6 t7 --config-file $config --debug-cache=shared,workspace-local \
  >   2>&1 | grep "_build/default/t[0-9]"
  Workspace-local cache miss: _build/default/t1: never seen this target before
  Shared cache miss [c3b01fa00405cec26e2767089ce95833] (_build/default/t1): not found in cache
  Workspace-local cache miss: _build/default/t2: never seen this target before
  Shared cache miss [2192c0618b09db243eebdd1b6dfc1dd2] (_build/default/t2): not found in cache
  Workspace-local cache miss: _build/default/t3: never seen this target before
  Shared cache miss [6b1cf0b588fb20352f65aeac747a5335] (_build/default/t3): not found in cache
  Workspace-local cache miss: _build/default/t5: never seen this target before
  Shared cache miss [d7f22228edc111b99a5baa35579754d0] (_build/default/t5): not found in cache
  Workspace-local cache miss: _build/default/t6: never seen this target before
  Shared cache miss [3cca934ba72987ac45bc451b1ef345b6] (_build/default/t6): not found in cache
