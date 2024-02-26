`melange.emit` that tries to use two implementations for the same virtual lib

  $ dune build @melange
  Error: Conflicting implementations for virtual library "vlib" in
  _build/default/vlib:
  - "impl1" in _build/default/impl1
    -> required by library "bar" in _build/default
  - "impl2" in _build/default/impl2
  This cannot work.
  -> required by melange target output
  -> required by alias melange
  Error: Conflicting implementations for virtual library "vlib" in
  _build/default/vlib:
  - "impl1" in _build/default/impl1
    -> required by library "bar" in _build/default
  - "impl2" in _build/default/impl2
  This cannot work.
  -> required by melange target output
  -> required by _build/default/output/foo.js
  -> required by alias melange
  [1]
