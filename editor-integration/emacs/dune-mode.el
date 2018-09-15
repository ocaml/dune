;;; dune-mode.el --- Mode for editing dune files   -*- coding: utf-8 -*-

;; Copyright (C) 2017- Christophe Troestler

;; This file is not part of GNU Emacs.

;; Permission to use, copy, modify, and distribute this software for
;; any purpose with or without fee is hereby granted, provided that
;; the above copyright notice and this permission notice appear in
;; all copies.
;;
;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
;; CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
;; LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
;; NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
;; CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(defvar dune-mode-hook nil
  "Hooks for the `dune-mode'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                     Syntax highlighting

(defface dune-error-face
  '((t (:foreground "yellow" :background "red" :bold t)))
  "Face for errors (e.g. obsolete constructs).")

(defvar dune-error-face 'dune-error-face
  "Face for errors (e.g. obsolete constructs).")

(defconst dune-keywords-regex
  (eval-when-compile
    (concat (regexp-opt
             '("jbuild_version" "library" "executable" "executables" "rule"
               "ocamllex" "ocamlyacc" "menhir" "alias" "install"
               "copy_files" "copy_files#" "include")
             ) "\\(?:\\_>\\|[[:space:]]\\)"))
  "Keywords in dune files.")

(defconst dune-fields-regex
  (eval-when-compile
    (regexp-opt
     '("name" "public_name" "synopsis" "modules" "libraries" "wrapped"
       "inline_tests" "inline_tests.backend"
       "preprocess" "preprocessor_deps" "optional" "c_names" "cxx_names"
       "install_c_headers" "modes" "no_dynlink" "kind"
       "ppx_runtime_libraries" "virtual_deps" "js_of_ocaml" "flags"
       "ocamlc_flags" "ocamlopt_flags" "library_flags" "c_flags"
       "cxx_flags" "c_library_flags" "self_build_stubs_archive"
       "modules_without_implementation"
       ;; + for "executable" and "executables":
       "package" "link_flags" "modes" "names" "public_names"
       ;; + for "rule":
       "targets" "action" "deps" "mode"
       ;; + for "menhir":
       "merge_into"
       ;; + for "install"
       "section" "files" "lib" "libexec" "bin" "sbin" "toplevel" "share"
       "share_root" "etc" "doc" "stublibs" "man" "misc")
     'symbols))
  "Field names allowed in dune files.")

(defvar dune-builtin-regex
  (eval-when-compile
    (concat (regexp-opt
             '(;; Actions
               "run" "chdir" "setenv"
               "with-stdout-to" "with-stderr-to" "with-outputs-to"
               "ignore-stdout" "ignore-stderr" "ignore-outputs"
               "progn" "echo" "write-file" "cat" "copy" "copy#" "system"
               "bash" "diff" "diff?"
               ;; inline_tests and inline_tests.backend
               ;; FIXME: "flags" is already a field and we do not have enough
               ;; context to distinguishing both.
               "backend" "generate_runner" "runner_libraries" "flags"
               "extends")
             t)
            "\\(?:\\_>\\|[[:space:]]\\)"))
  "Builtin sub-fields in dune")

(defvar dune-var-kind-regex
  (eval-when-compile
    (regexp-opt
     '("path" "path-no-dep" "exe" "bin" "lib" "libexec" "lib-available"
       "version" "read" "read-lines" "read-strings")
     'words))
  "Optional prefix to variable names.")

(defvar dune-var-regex
      (concat "\\(!?\\)\\(\\(?:" dune-var-kind-regex
              ":\\)?\\)\\([a-zA-Z][a-zA-Z0-9_.-]*\\|[<@^]\\)"
              "\\(\\(?::[a-zA-Z][a-zA-Z0-9_.-]*\\)?\\)"))

(defmacro dune--field-vals (field &rest vals)
  `(list (concat "(" ,field "[[:space:]]+" ,(regexp-opt vals t))
         1 font-lock-constant-face))

(setq dune-font-lock-keywords
  `((,dune-keywords-regex . font-lock-keyword-face)
    (,(concat "(" dune-fields-regex) 1 font-lock-function-name-face)
    ("\\(true\\|false\\)" 1 font-lock-constant-face)
    ("(\\(select\\)[[:space:]]+[^[:space:]]+[[:space:]]+\\(from\\)\\>"
     (1 font-lock-constant-face)
     (2 font-lock-constant-face))
    ,(eval-when-compile
       (dune--field-vals "kind" "normal" "ppx_rewriter" "ppx_deriver"))
    ,(eval-when-compile
       (dune--field-vals "mode" "standard" "fallback" "promote"
                                "promote-until-clean"))
    (,(concat "(" dune-builtin-regex) 1 font-lock-builtin-face)
    ("(preprocess[[:space:]]+(\\(pps\\)" 1 font-lock-builtin-face)
    (,(eval-when-compile
        (concat "(" (regexp-opt '("fallback") t)))
     1 dune-error-face)
    (,(concat "${" dune-var-regex "}")
     (1 dune-error-face)
     (2 font-lock-builtin-face)
     (4 font-lock-variable-name-face)
     (5 font-lock-variable-name-face))
    (,(concat "$(" dune-var-regex ")")
     (1 dune-error-face)
     (2 font-lock-builtin-face)
     (4 font-lock-variable-name-face)
     (5 font-lock-variable-name-face))
    ("\\(:[a-zA-Z]+\\)\\b" 1 font-lock-builtin-face)))

(defvar dune-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\; "< b" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    table)
  "dune syntax table.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                             SMIE

(require 'smie)

(defvar dune-smie-grammar
  (when (fboundp 'smie-prec2->grammar)
    (smie-prec2->grammar
     (smie-bnf->prec2 '()))))

(defun dune-smie-rules (kind token)
  (cond
   ((eq kind :close-all) '(column . 0))
   ((and (eq kind :after) (equal token ")"))
    (save-excursion
      (goto-char (cadr (smie-indent--parent)))
      (if (looking-at-p dune-keywords-regex)
          '(column . 0)
        1)))
   ((eq kind :before)
    (if (smie-rule-parent-p "(")
        (save-excursion
          (goto-char (cadr (smie-indent--parent)))
          (cond
           ((looking-at-p dune-keywords-regex) 1)
           ((looking-at-p dune-fields-regex)
            (smie-rule-parent 0))
           ((smie-rule-sibling-p) (cons 'column (current-column)))
           (t (cons 'column (current-column)))))
      '(column . 0)))
   (t 1)))

(defun verbose-dune-smie-rules (kind token)
  (let ((value (dune-smie-rules kind token)))
    (message
     "%s '%s'; sibling-p:%s parent:%s hanging:%s = %s"
     kind token
     (ignore-errors (smie-rule-sibling-p))
     (ignore-errors smie--parent)
     (ignore-errors (smie-rule-hanging-p))
     value)
    value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar dune-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'compile)
    map)
  "Keymap used in dune mode.")

;;;###autoload
(define-derived-mode dune-mode prog-mode "dune"
  "Major mode to edit dune files."
  (setq-local font-lock-defaults '(dune-font-lock-keywords))
  (setq-local comment-start ";")
  (setq-local comment-end "")
  (setq indent-tabs-mode nil)
  (setq-local require-final-newline mode-require-final-newline)
  (smie-setup dune-smie-grammar #'dune-smie-rules)
  (dune-build-menu)
  (run-mode-hooks 'dune-mode-hook))


;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\(?:\\`\\|/\\)dune\\(?:\\.inc\\)?\\'" . dune-mode))


(provide 'dune-mode)
