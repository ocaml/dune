(** Wrapper for install entries that uses sites *)

open Import

val make_with_site
  :  Section_with_site.t
  -> ?dst:string
  -> (loc:Loc.t -> pkg:Package.Name.t -> site:Site.t -> Section.t Memo.t)
  -> kind:Install.Entry.kind
  -> Path.Build.t
  -> Path.Build.t Install.Entry.t Memo.t

(** Same as Entry, but the destination can be in the site of a package *)
module Entry_with_site : sig
  type 'src t =
    { src : 'src
    ; dst : Install.Entry.Dst.t
    ; section : Section_with_site.t
    }
end
