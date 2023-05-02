(** configurator rules *)

(** Generate the rules for producing the files needed by configurator. *)
val gen_rules : Context.t -> unit Memo.t

(** Force the files required by configurator at runtime to be produced. *)
val force_files : unit Memo.Lazy.t
