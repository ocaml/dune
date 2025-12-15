open Stdune
module Process = Dune_engine.Process
open Fiber.O

(** {1 Archive Extraction}

    This module handles extraction of .tar, .tar.gz, .tar.bz2, and .zip archives
    using system tools (tar, bsdtar, unzip). The main complexity comes from
    supporting different tar implementations with varying capabilities:

    - BSD tar (libarchive): Auto-detects compression, supports zip extraction
    - GNU tar: Auto-detects compression, no zip support
    - Other (OpenBSD, busybox): Requires explicit -z/-j flags, no zip support

    We use GADTs with polymorphic variants to encode these capabilities at the
    type level. [Tar.t] has a type parameter indicating which formats it supports,
    and [Tar.args] can only be called with formats valid for that tar. This
    prevents invalid combinations (like extracting zip with GNU tar) at compile
    time rather than runtime.

    For zip extraction, we prefer BSD tar when available, falling back to the
    [unzip] binary if BSD tar is not found. *)

module Format : sig
  (** Supported archive formats:
      - [`Tar]: uncompressed tar (.tar)
      - [`Tar_gz]: gzip-compressed tar (.tar.gz, .tgz)
      - [`Tar_bz2]: bzip2-compressed tar (.tar.bz2, .tbz)
      - [`Zip]: zip archive (.zip) *)
  type t =
    [ `Tar
    | `Tar_gz
    | `Tar_bz2
    | `Zip
    ]

  (** Detect archive format from filename extension. Returns the format and
      the matched extension. *)
  val of_filename : Filename.t -> (t * string) option
end = struct
  type t =
    [ `Tar
    | `Tar_gz
    | `Tar_bz2
    | `Zip
    ]

  let of_filename =
    let extensions =
      [ ".tar", `Tar
      ; ".tar.gz", `Tar_gz
      ; ".tgz", `Tar_gz
      ; ".tar.bz2", `Tar_bz2
      ; ".tbz", `Tar_bz2
      ; ".zip", `Zip
      ]
    in
    fun filename ->
      let check_suffix suffix = Filename.check_suffix filename suffix in
      List.find_map extensions ~f:(fun (ext, format) ->
        Option.some_if (check_suffix ext) (format, ext))
  ;;
end

let is_supported filename = Option.is_some (Format.of_filename filename)
let which bin_name = Bin.which ~path:(Env_path.path Env.initial) bin_name

module Error = struct
  type t =
    | No_extractor of
        { ext : string
        ; tried : string list
        }
    | Command_failed of
        { bin : Path.t
        ; archive : Path.t
        ; exit_code : int
        ; stderr : string list
        }
    | Read_dir_failed of
        { archive : Path.t
        ; error : Unix_error.Detailed.t
        }

  let message = function
    | No_extractor { ext; tried } ->
      [ Pp.textf "No program found to extract %s files. Tried:" ext
      ; Pp.enumerate tried ~f:Pp.verbatim
      ]
    | Command_failed { bin; archive; exit_code; stderr } ->
      [ Pp.textf "Failed to extract '%s':" (Path.basename archive)
      ; Pp.hovbox
          (Pp.concat
             ~sep:Pp.space
             [ User_message.command (Path.basename bin)
             ; Pp.textf "failed with non-zero exit code '%d' and output:" exit_code
             ])
      ; Pp.vbox (Pp.concat_map ~sep:Pp.cut ~f:Pp.paragraph stderr)
      ]
    | Read_dir_failed { archive; error } ->
      [ Pp.textf "Failed to extract '%s':" (Path.to_string_maybe_quoted archive)
      ; Pp.text (Unix_error.Detailed.to_string_hum error)
      ]
  ;;

  let raise t = User_error.raise (message t)
end

module Tar : sig
  (** Tar executable for current system. The type parameter encodes which
      formats this tar supports - pattern match on [found] to get the right
      type.

 {v
 | Kind  | Implementations          | Detects compression | Zip support |
 |-------|--------------------------|---------------------|-------------|
 | Bsd   | bsdtar, libarchive       | yes                 | yes         |
 |       | (macOS, FreeBSD, Win10+) |                     |             |
 |       |                          |                     |             |
 | Gnu   | GNU tar (most Linux)     | yes                 | no          |
 |       |                          |                     |             |
 | Other | OpenBSD tar, busybox tar | no                  | no          |
 v}

      [Other] is the fallback for unknown implementations. We use explicit
      [-z]/[-j] flags which is the safest approach. *)
  type 'fmt t

  (** Archive formats supported by BSD tar. *)
  type bsd =
    [ `Tar
    | `Tar_gz
    | `Tar_bz2
    | `Zip
    ]

  (** Result of finding tar. Pattern match to get the appropriately typed tar. *)
  type found =
    | Bsd of bsd t
    | Gnu of [ `Tar | `Tar_gz | `Tar_bz2 ] t
    | Other of [ `Tar | `Tar_gz | `Tar_bz2 ] t

  (** Attempt to find tar executable for current system. *)
  val find : found option Fiber.Lazy.t

  (** Names of binaries checked when attempting to find tar executable. *)
  val bin_names : string list

  (** Path to tar executable. *)
  val path : 'a t -> Path.t

  (** [args t format ~archive ~target] provides the arguments for extraction.
      The [format] must be in the set of formats supported by [t]. *)
  val args : 'fmt t -> format:'fmt -> archive:Path.t -> target:Path.t -> string list
end = struct
  type _ kind =
    | K_bsd : [ `Tar | `Tar_gz | `Tar_bz2 | `Zip ] kind
    | K_gnu : [ `Tar | `Tar_gz | `Tar_bz2 ] kind
    | K_other : [ `Tar | `Tar_gz | `Tar_bz2 ] kind

  type 'fmt t =
    { path : Path.t
    ; kind : 'fmt kind
    }

  type bsd =
    [ `Tar
    | `Tar_gz
    | `Tar_bz2
    | `Zip
    ]

  type found =
    | Bsd of bsd t
    | Gnu of [ `Tar | `Tar_gz | `Tar_bz2 ] t
    | Other of [ `Tar | `Tar_gz | `Tar_bz2 ] t

  (** The order of binaries we should try from most capable to least + Windows. *)
  let bin_names = [ "bsdtar"; "tar"; "gtar"; "tar.exe" ]

  let find =
    Fiber.Lazy.create (fun () ->
      match List.find_map bin_names ~f:which with
      | None -> Fiber.return None
      | Some path ->
        let+ output, _ = Process.run_capture ~display:Quiet Return path [ "--version" ] in
        let matches s = Re.execp (Re.compile (Re.str s)) output in
        Some
          (if matches "bsdtar" || matches "libarchive"
           then Bsd { path; kind = K_bsd }
           else if matches "GNU tar"
           then Gnu { path; kind = K_gnu }
           else Other { path; kind = K_other }))
  ;;

  let path t = t.path

  let args (type fmt) (t : fmt t) ~(format : fmt) ~archive ~target =
    let decompress_flag =
      match t.kind, format with
      | K_bsd, (`Tar | `Tar_gz | `Tar_bz2 | `Zip) -> []
      | K_gnu, (`Tar | `Tar_gz | `Tar_bz2) -> []
      | K_other, `Tar -> []
      | K_other, `Tar_gz -> [ "-z" ]
      | K_other, `Tar_bz2 -> [ "-j" ]
    in
    [ "-x" ]
    @ decompress_flag
    @ [ "-f"; Path.to_string archive; "-C"; Path.to_string target ]
  ;;
end

module Unzip : sig
  (** A binary that can extract zip files. *)
  type t

  (** Find a binary that can extract zip files. *)
  val find : t option Fiber.Lazy.t

  (** Names of binaries checked when attempting to find binary that can extract
      zip files. *)
  val bin_names : string list

  (** Path to binary that can extract zip files. *)
  val path : t -> Path.t

  (** Arguments given to binary that can extract zip files. *)
  val args : t -> archive:Path.t -> target:Path.t -> string list
end = struct
  type t =
    | Unzip of Path.t
    | Tar of Tar.bsd Tar.t

  let bin_names = Tar.bin_names @ [ "unzip" ]

  let find =
    (* We first check if we have a tar binary capable of unzipping, if not we
       fallback to unzip. *)
    Fiber.Lazy.create (fun () ->
      Fiber.Lazy.force Tar.find
      >>| function
      | Some (Bsd tar) -> Some (Tar tar)
      | Some (Gnu _ | Other _) | None ->
        (match which "unzip" with
         | Some path -> Some (Unzip path)
         | None -> None))
  ;;

  let path = function
    | Unzip path -> path
    | Tar tar -> Tar.path tar
  ;;

  let args t ~archive ~target =
    match t with
    | Unzip _ -> [ Path.to_string archive; "-d"; Path.to_string target ]
    | Tar tar -> Tar.args tar ~format:`Zip ~archive ~target
  ;;
end

module Extractor : sig
  (** [Extractor.run ~archive ~target] extracts the given [archive] using an
      appropriate tool to do so. *)
  val run : archive:Path.t -> target:Path.t -> (unit, Error.t) result Fiber.t
end = struct
  let no_extractor_error ~ext ~bin_names =
    Fiber.return @@ Error (Error.No_extractor { ext; tried = bin_names })
  ;;

  let run_cmd ~bin ~args ~archive =
    let temp_stderr = Temp.create File ~prefix:"extract" ~suffix:"stderr" in
    Fiber.finalize ~finally:(fun () ->
      Temp.destroy File temp_stderr;
      Fiber.return ())
    @@ fun () ->
    let+ (), exit_code =
      Process.run
        ~display:Quiet
        ~stdout_to:
          (Process.Io.make_stdout ~output_on_success:Swallow ~output_limit:1_000_000)
        ~stderr_to:(Process.Io.file temp_stderr Out)
        Return
        bin
        args
    in
    if exit_code <> 0
    then
      Io.with_file_in temp_stderr ~f:(fun err_channel ->
        let stderr = Io.input_lines err_channel in
        Error (Error.Command_failed { bin; archive; exit_code; stderr }))
    else Ok ()
  ;;

  let run ~archive ~target =
    let format, ext =
      Format.of_filename (Path.to_string archive) |> Option.value ~default:(`Tar, ".tar")
    in
    match (format : Format.t) with
    | `Zip ->
      Fiber.Lazy.force Unzip.find
      >>= (function
       | Some unzip ->
         let bin = Unzip.path unzip in
         let args = Unzip.args unzip ~archive ~target in
         run_cmd ~bin ~args ~archive
       | None -> no_extractor_error ~ext ~bin_names:Unzip.bin_names)
    | (`Tar | `Tar_gz | `Tar_bz2) as format ->
      Fiber.Lazy.force Tar.find
      >>= (function
       | Some (Bsd tar) ->
         let bin = Tar.path tar in
         let args = Tar.args tar ~format ~archive ~target in
         run_cmd ~bin ~args ~archive
       | Some (Gnu tar | Other tar) ->
         let bin = Tar.path tar in
         let args = Tar.args tar ~format ~archive ~target in
         run_cmd ~bin ~args ~archive
       | None -> no_extractor_error ~ext ~bin_names:Tar.bin_names)
  ;;
end

(** Make extraction atomic by extracting to a temp directory and renaming. *)
let extract ~archive ~target =
  let target_in_temp =
    let prefix = Path.basename target in
    let suffix = Path.basename archive in
    Temp_dir.dir_for_target ~target ~prefix ~suffix
  in
  Fiber.finalize ~finally:(fun () ->
    Temp.destroy Dir target_in_temp;
    Fiber.return ())
  @@ fun () ->
  Path.mkdir_p target_in_temp;
  Extractor.run ~archive ~target:target_in_temp
  >>| function
  | Error _ as e -> e
  | Ok () ->
    (* Opam expects only one top-level directory and then strips it. We employ a
       similar heuristic here. Opam is stricter in the multiple top-level entries
       case since it errors, whereas we use the extraction directory as-is. *)
    let target_in_temp =
      match Path.readdir_unsorted_with_kinds target_in_temp with
      | Error error -> Error (Error.Read_dir_failed { archive; error })
      | Ok [ (fname, S_DIR) ] -> Ok (Path.relative target_in_temp fname)
      | Ok _ -> Ok target_in_temp
    in
    (match target_in_temp with
     | Error _ as e -> e
     | Ok target_in_temp ->
       (* CR-soon Alizter: this might already exist, don't do pointeless work *)
       Path.mkdir_p (Path.parent_exn target);
       (* CR-someday Alizter: Add fallback to copy if EXDEV raised here *)
       Path.rename target_in_temp target;
       Ok ())
;;

let extract_exn ~archive ~target =
  extract ~archive ~target
  >>| function
  | Ok () -> ()
  | Error e -> Error.raise e
;;
