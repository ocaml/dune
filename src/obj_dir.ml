open! Stdune
open Import

type t =
  { dir: Path.t (** The source_root directory *)
  ; public_dir: Path.t  (** The public compiled file directory *)
  ; private_dir: Path.t option   (** The private compiled file directory *)
  }

let all_objs_dir t = (t.public_dir::(Option.to_list t.private_dir))

let make ~dir ~public_dir ?private_dir () =
  { dir
  ; public_dir
  ; private_dir
  }


let make_local ~dir ~has_private_modules lib_name =
  let obj_dir = Utils.library_object_directory ~dir lib_name in
  let private_dir =
    Option.some_if
      has_private_modules
      (Utils.library_private_obj_dir ~obj_dir)
  in
  make ~dir ~public_dir:obj_dir ?private_dir ()

let make_exe ~dir exe_name =
  let obj_dir = Utils.executable_object_directory ~dir exe_name in
  make ~dir ~public_dir:obj_dir ?private_dir:None ()

let make_external ~dir =
  { dir
  ; public_dir = dir
  ; private_dir = None
  }

let to_sexp { dir; public_dir; private_dir } =
  let open Sexp.Encoder in
  record
    [ "dir", Path.to_sexp dir
    ; "public_dir", Path.to_sexp public_dir
    ; "private_dir", (option Path.to_sexp) private_dir
    ]

let pp fmt { dir; public_dir; private_dir } =
  Fmt.record fmt
    [ "dir", Fmt.const Path.pp dir
    ; "public_dir", Fmt.const Path.pp public_dir
    ; "private_dir", Fmt.const (Fmt.optional Path.pp) private_dir
    ]


let encode
      { dir
      ; public_dir
      ; private_dir
      } =
  let open Dune_lang.Encoder in
  let extract d =
    Path.descendant ~of_:dir d
    |> Option.value_exn
    |> Path.to_string
  in
  let public_dir = extract public_dir in
  let private_dir = Option.map ~f:extract private_dir in
  record_fields
    [ field "public_dir" ~equal:String.equal ~default:"." string public_dir
    ; field_o "private_dir" string private_dir
    ]


let decode ~dir =
  let open Dune_lang.Decoder in
  fields (
    let%map public_dir = field ~default:"." "public_dir" string
    and private_dir = field_o "private_dir" string
    in
    make ~dir
      ~public_dir:(Path.relative dir public_dir)
      ?private_dir:(Option.map ~f:(Path.relative dir) private_dir)
      ()
  )
