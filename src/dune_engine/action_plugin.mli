module Server : sig
  val implement_handler : 'a Root.Rpc.Server.Handler.t -> unit
end

val action : prog:Action.Prog.t -> args:string list -> Action.t
