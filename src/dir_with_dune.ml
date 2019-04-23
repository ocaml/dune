open Stdune

type 'data t =
  { src_dir         : Path.Source.t
  ; ctx_dir         : Path.t
  ; data            : 'data
  ; scope           : Scope.t
  ; kind            : Dune_lang.File_syntax.t
  ; dune_version    : Syntax.Version.t
  }
