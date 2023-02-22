open Stdune

module type S = sig
  type t =
    { st_dev : int  (** Device number *)
    ; st_ino : int  (** Inode number *)
    ; st_kind : File_kind.t  (** Kind of the file *)
    }
end
