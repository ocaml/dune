open Stdune

type 'data t =
  { dir    : Path.t
  ; scope  : Scope.t
  ; data   : 'data
  ; kind   : Dune_lang.Syntax.t
  }
