@ocaml-index
============

This alias corresponds to the set of targets necessary for development tools to
provide project-wide queries such as "get all references of this value". These
targets are indexes built using the required `ocaml-index` binary. Since this
alias also incudes the ``*.cmi``, ``*.cmt``, and ``*.cmti`` files usually built
by ``check``, it can be used in most projects as a replacement to get a fast
feedback loop while maintaining the indexes up-to-date.
