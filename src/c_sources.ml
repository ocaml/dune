open Stdune
open Dune_file

module Library = Dune_file.Library

module Files = struct
  type 'a t =
    { c   : 'a
    ; cxx : 'a
    }

  let empty =
    { c = String.Set.empty
    ; cxx = String.Set.empty
    }

  let make ~files =
    String.Set.fold files ~init:empty ~f:(fun fn acc ->
      match String.lsplit2 fn ~on:'.' with
      | Some (_, "c") ->
        { acc with c = String.Set.add acc.c fn }
      | Some (_, "cpp") ->
        { acc with cxx = String.Set.add acc.cxx fn }
      | _ -> acc)

  let foreign_objects { c; cxx } ~dir ~ext_obj =
    String.Map.(keys c @ keys cxx)
    |> List.map ~f:(fun c -> Path.relative dir (c ^ ext_obj))
end

type t =
  { libraries : (Loc.t * Path.t) String.Map.t Files.t Lib_name.Map.t
  }

let for_lib t ~dir ~name =
  match Lib_name.Map.find t.libraries name with
  | Some m -> m
  | None ->
    Exn.code_error "C_sources.for_lib"
      [ "name", Lib_name.to_sexp name
      ; "dir", Path.to_sexp dir
      ; "available", Sexp.Encoder.(list Lib_name.to_sexp)
                       (Lib_name.Map.keys t.libraries)
      ]

let empty =
  { libraries = Lib_name.Map.empty
  }

let c_name, cxx_name =
  let make what ext ~loc s =
    if match s with
      | "" | "." | ".."  -> true
      | _ -> false then
      Errors.fail loc
        "%S is not a valid %s name."
        s what what ext
    else
      s
  in
  ( make "C"   "c"
  , make "C++" "cpp"
  )

module Eval = struct
  module Value = struct
    type t = string
    type key = string
    let key s = s
  end

  include Ordered_set_lang.Make_loc(String)(Value)
end

let make (d : _ Dir_with_dune.t) ~(c_files : String.Set.t Files.t) =
  let libs =
    List.filter_map d.data ~f:(fun stanza ->
      match (stanza : Stanza.t) with
      | Library lib ->
        let eval ext validate (files : String.Set.t) osl =
          Eval.eval_unordered osl
            ~parse:validate
            ~standard:String.Map.empty
          |> String.Map.map ~f:(fun (loc, s) ->
            let fn = s ^ ext in
            if String.Set.mem files fn then
              (loc, Path.relative d.ctx_dir fn)
            else
              Errors.fail loc "%s does not exist" fn
          )
        in
        let names =
          Option.value ~default:Ordered_set_lang.standard in
        let c = eval ".c" c_name c_files.c (names lib.c_names) in
        let cxx = eval ".cpp" cxx_name c_files.cxx (names lib.cxx_names) in
        Some (lib, { Files. c ; cxx })
      | _ -> None
    )
  in
  let libraries =
    match
      Lib_name.Map.of_list_map libs ~f:(fun (lib, m) ->
        Library.best_name lib, m)
    with
    | Ok x -> x
    | Error (name, _, (lib2, _)) ->
      Errors.fail lib2.buildable.loc
        "Library %a appears for the second time \
         in this directory"
        Lib_name.pp_quoted name
  in
  { libraries
  }
