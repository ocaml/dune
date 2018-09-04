;;; dune.el --- Integration with the dune build system

;; Copyright 2018 Jane Street Group, LLC <opensource@janestreet.com>
;; URL: https://github.com/ocaml/dune
;; Version: 1.0

;;; Commentary:

;; This package provides helper functions for interacting with the
;; dune build system from Emacs.

;; Installation:
;; You need to install the OCaml program ``dune''.  The
;; easiest way to do so is to install the opam package manager:
;;
;;   https://opam.ocaml.org/doc/Install.html
;;
;; and then run "opam install dune".

;;; Code:

(defgroup dune nil
  "Integration with the dune build system."
  :tag "Dune build system."
  :version "1.0")

(defcustom dune-command "dune"
  "The dune command."
  :type 'string)

;;;###autoload
(defun dune-promote ()
  "Promote the correction for the current file."
  (interactive)
  (if (buffer-modified-p)
      (error "Cannot promote as buffer is modified")
    (shell-command
     (format "%s promote %s"
             dune-command
             (file-name-nondirectory (buffer-file-name))))
    (revert-buffer nil t)))

;;;###autoload
(defun dune-runtest-and-promote ()
  "Run tests in the current directory and promote the current buffer."
  (interactive)
  (compile (format "%s build @@runtest" dune-command))
  (dune-promote))

(provide 'dune)

;;; dune.el ends here
