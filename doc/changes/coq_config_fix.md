- [coq rules] Be more tolerant when coqc --print-version / --config
  don't work properly, and fallback to a reasonable default. This
  fixes problems when building Coq projects with `(stdlib no)` and
  likely other cases. (#8966, fix #8958, @Alizter, reported by Lasse
  Blaauwbroek)

