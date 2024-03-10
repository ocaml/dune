open Import

val isn't_allowed_in_this_position : source:Dune_lang.Template.Pform.t -> 'a
val as_in_build_dir : what:string -> loc:Loc.t -> Path.t -> Path.Build.t

module type S = sig
  type t

  val project : t -> Dune_project.t
  val eval_blang : t -> Blang.t -> bool Memo.t
end

include S

val set_db : (dir:Path.Build.t -> t Memo.t) -> unit
val get : dir:Path.Build.t -> t Memo.t
val project : t -> Dune_project.t Memo.t
val create : 'a Memo.t -> (module S with type t = 'a) -> t
