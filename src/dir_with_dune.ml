open Stdune

type 'data t =
  { src_dir         : Path.Source.t
  ; ctx_dir         : Path.t
  ; data            : 'data
  ; scope           : Scope.t
  ; kind            : Dune_lang.Syntax.t
  ; dune_version    : Syntax.Version.t
  }
