Melange stanza does not support consuming a library being placed outside the stanza folder

  $ dune build
  Error: The library lib is used by a melange.emit stanza but the library
  folder _build/default/lib is not a descendant of the stanza folder
  _build/default/inside
  -> required by alias default
  [1]
