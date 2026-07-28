open Import

type failure =
  | Checksum_mismatch of Checksum.t
  | Unavailable of User_message.t option

(** [fetch ~checksum ~archive_mirrors ~target url] will fetch [url] into
    [target]. When [checksum] is present, checksum-addressed objects under
    [archive_mirrors] are tried before [url]. Local (file) URLs are read
    directly and never consult mirrors. Every downloaded file is verified
    against [checksum], unless [checksum] is [None].

    return [Error (Checksum_mismatch _)] When the downloaded file doesn't match
    the expected [checksum], this will pass the actually computed checksum.

    return [Error (Unavailable _))] When the file can't be retrieved, e.g. not
    available at the location. *)
val fetch
  :  unpack:bool
  -> checksum:Checksum.t option
  -> archive_mirrors:OpamUrl.t list
  -> target:Path.t
  -> url:Loc.t * OpamUrl.t
  -> (unit, failure) result Fiber.t

val fetch_without_checksum
  :  unpack:bool
  -> target:Path.t
  -> url:Loc.t * OpamUrl.t
  -> (unit, User_message.t option) result Fiber.t

val fetch_git
  :  Rev_store.t
  -> target:Path.t
  -> url:Loc.t * OpamUrl.t
  -> (unit, failure) result Fiber.t
