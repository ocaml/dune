module Type = struct
  type windows = Windows
  type unix = Unix
  type 'a t =
    | Windows : windows t
    | Unix : unix t
end

module type S = sig
  type t
  val t : t Type.t
end

module Windows : S = struct
  type t = Type.windows
  let t = Type.Windows
end

module Unix : S = struct
  type t = Type.unix
  let t = Type.Unix
end

include (val (
  if Sys.win32 then
    (module Windows)
  else
    (module Unix)
) : S)
