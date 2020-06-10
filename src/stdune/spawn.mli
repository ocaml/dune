(** Subset of https://github.com/janestreet/spawn/blob/master/src/spawn.mli *)

val spawn :
     ?env:Env.t
  -> prog:string
  -> argv:string list
  -> ?stdin:Unix.file_descr
  -> ?stdout:Unix.file_descr
  -> ?stderr:Unix.file_descr
  -> unit
  -> Pid.t
