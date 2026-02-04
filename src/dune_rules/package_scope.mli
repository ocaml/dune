open Import

type t = Package.Name.Set.t Path.Source.Map.t

val db : unit -> t Memo.t
val find_scope_for_dir : t -> Path.Source.t -> (Path.Source.t * Package.Name.Set.t) option
val packages : unit -> Package.t Package.Name.Map.t Memo.t
val mask : unit -> Only_packages.t Memo.t
