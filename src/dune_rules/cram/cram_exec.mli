open Import

(** Produces the script containing only the commands to run *)
val make_script : src:Path.t -> script:Path.Build.t -> Action.t

(** Runs the script created in [make_script] *)
val run
  :  src:Path.t
  -> dir:Path.t
  -> script:Path.t
  -> output:Path.Build.t
  -> timeout:(Loc.t * float) option
  -> Action.t

(** Produces a diff if [src] needs to be updated *)
val diff : src:Path.t -> output:Path.t -> Action.t

(** Corresponds the user written cram action *)
val action : Path.t -> Action.t

module For_tests : sig
  val cram_stanzas : Lexing.lexbuf -> string list Cram_lexer.block list
  val dyn_of_block : string list Cram_lexer.block -> Dyn.t
end
