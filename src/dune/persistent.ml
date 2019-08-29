open Stdune

module type Desc = sig
  type t

  val name : string

  val version : int
end

module Make (D : Desc) = struct
  let magic = sprintf "DUNE-%sv%d:" D.name D.version

  let dump file (v : D.t) =
    Io.with_file_out file ~f:(fun oc ->
        output_string oc magic;
        Marshal.to_channel oc v [])

  let load file =
    if Path.exists file then
      Io.with_file_in file ~f:(fun ic ->
          match really_input_string ic (String.length magic) with
          | exception End_of_file -> None
          | s ->
            if s = magic then
              Some (Marshal.from_channel ic : D.t)
            else
              None)
    else
      None
end
