open Stdune

(** A directory with a [dune] file *)
type 'data t =
  { src_dir         : Path.t
  ; ctx_dir         : Path.t  (** [_build/context-name/src_dir] *)
  ; data            : 'data
  ; scope           : Scope.t
  ; kind            : Dune_lang.Syntax.t
  ; dune_version    : Syntax.Version.t
  }
