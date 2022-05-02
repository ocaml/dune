open! Dune_engine

let library_path = []

let roots =
  Install.Section.Paths.Roots.
    { lib_root = None
    ; man = None
    ; doc_root = None
    ; etc_root = None
    ; share_root = None
    ; bin = None
    ; sbin = None
    ; libexec_root = None
    }
