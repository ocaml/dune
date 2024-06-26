open Import

type failure =
  | Checksum_mismatch of
      { expected : Checksum.t
      ; got : Checksum.t
      }
  | Unavailable of User_message.t option

(** [fetch ~checksums ~target url] will fetch [url] into [target]. It will verify
    the downloaded file against [checksums], unless it [checksums] is [None].

    return [Error (Checksum_mismatch _)] When the downloaded file doesn't match
    the expected [checksums], this will return the actually computed checksum.

    return [Error (Unavailable _))] When the file can't be retrieved, e.g. not
    available at the location. *)
val fetch
  :  unpack:bool
  -> checksums:Checksum.t list
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
