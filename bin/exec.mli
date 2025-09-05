open Import

module Cmd_arg : sig
  type t

  val conv : t Arg.conv
  val expand : t -> root:Workspace_root.t -> sctx:Super_context.t -> string Memo.t
end

(** Returns the path to the executable [prog] as it will be resolved by dune:
  - if [prog] is the name of an executable defined by the project then the path
    to that executable will be returned, and evaluating the returned memo will
    build the executable if necessary.
  - otherwise if [prog] is the name of an executable in the "bin" directory of
    a package in this project's dependency cone then the path to that executable
    file will be returned. Note that for this reason all dependencies of the
    project will be built when the returned memo is evaluated (unless the first
    case is hit).
  - otherwise if [prog] is the name of an executable in one of the directories
    listed in the PATH environment variable, the path to that executable will be
    returned. *)
val get_path : Common.t -> Super_context.t -> prog:string -> Path.t Memo.t

val command : unit Cmd.t
