Melange stanza does not support consuming a library being placed outside the stanza folder

  $ dune build
  Error: The library app is used from a melange.emit stanza but the library
  folder _build/default/app is not a descendant of the stanza folder
  _build/default/inside
  -> required by alias default
  [1]
