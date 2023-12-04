open Import

type failure =
  | Checksum_mismatch of Checksum.t
  | Unavailable of User_message.t option

(** [fetch ~checksum ~target url] will fetch [url] into [target]. It will verify
    the downloaded file against [checksum], unless it [checksum] is [None].

    @raise Checksum_mismatch
      When the downloaded file doesn't match the expected [checksum], this will
      pass the actually computed checksum.
    @raise Unavailable
      When the file can't be retrieved, e.g. not available at the location. *)
val fetch
  :  unpack:bool
  -> checksum:Checksum.t option
  -> target:Path.t
  -> OpamUrl.t
  -> (unit, failure) result Fiber.t
