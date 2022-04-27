open Stdune

module type Desc = sig
  type t

  val name : string

  val version : int

  val to_dyn : t -> Dyn.t
end

type data = ..

module type Desc_with_data = sig
  include Desc

  type data += T of t
end

let registry = String.Table.create 16

let max_magic_length = 128

let register (module D : Desc_with_data) =
  match String.Table.add registry D.name (module D : Desc_with_data) with
  | Ok () -> ()
  | Error _ ->
    Code_error.raise "Persistent file kind registered for the second time"
      [ ("name", String D.name) ]

module Make (D : Desc) = struct
  let magic = sprintf "DUNE-%sv%d:" D.name D.version

  let () =
    if String.length magic > max_magic_length then
      Code_error.raise "Persistent.Make: magic string too long"
        [ ("magic", String magic); ("max_magic_length", Int max_magic_length) ]

  type data += T of D.t

  let () =
    register
      (module struct
        include D

        type data += T = T
      end : Desc_with_data)

  let to_string (v : D.t) = Printf.sprintf "%s%s" magic (Marshal.to_string v [])

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
              match (Marshal.from_channel ic : D.t) with
              | exception Failure f ->
                Log.info_user_message
                  (User_message.make
                     [ Pp.tag User_message.Style.Warning
                         (Pp.textf "Failed to load corrupted file %s: %s"
                            (Path.to_string file) f)
                     ]);
                None
              | d -> Some d
            else None)
    else None
end

type t = T : (module Desc with type t = 'a) * 'a -> t

let load_exn path =
  Io.with_file_in path ~f:(fun ic ->
      let buf = Buffer.create max_magic_length in
      let rec read_magic n =
        if n = max_magic_length then None
        else
          match Stdlib.input_char ic with
          | exception End_of_file -> None
          | ':' -> Some (Buffer.contents buf)
          | c ->
            Buffer.add_char buf c;
            read_magic (n + 1)
      in
      let magic =
        let open Option.O in
        let* s = read_magic 0 in
        let* s = String.drop_prefix s ~prefix:"DUNE-" in
        let* name, version = String.rsplit2 s ~on:'v' in
        let* version = Int.of_string version in
        Some (name, version)
      in
      match magic with
      | None ->
        User_error.raise ~loc:(Loc.in_file path)
          [ Pp.text "This file is not a persistent dune file." ]
      | Some (name, version) -> (
        match String.Table.find registry name with
        | None ->
          User_error.raise ~loc:(Loc.in_file path)
            [ Pp.textf "Unknown type of persistent dune file: %s."
                (String.escaped name)
            ]
        | Some (module D : Desc_with_data) ->
          if D.version <> version then
            User_error.raise ~loc:(Loc.in_file path)
              [ Pp.textf "Unsupported version of dune '%s' file: %d."
                  (String.escaped name) version
              ; Pp.textf "I only know how to read version %d." D.version
              ];
          let data : D.t = Marshal.from_channel ic in
          T ((module D), data)))
