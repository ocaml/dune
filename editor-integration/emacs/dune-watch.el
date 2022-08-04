;;; dune-watch.el --- provides integration with dune watch  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Kiran Gopinathan
;; Author: Kiran Gopinathan <kirang@comp.nus.edu.sg>
;; Keywords: 

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package complements dune mode with integration with --watch
;; tasks.

;; Permission to use, copy, modify, and distribute this software for
;; any purpose with or without fee is hereby granted, provided that
;; the above copyright notice and this permission notice appear in
;; all copies.
;;
;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT SHALL THE
;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
;; CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
;; LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
;; NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
;; CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

;;; Code:
(require 'subr-x)

;;;; Customisation 
(defgroup dune-watch nil
  "Customisation group for dune-watch-minor-mode."
  :prefix "dune-watch-mode-"
  :group 'applications)
;;;;; Command configuration 

(defcustom dune-watch-default-command "build"
  "Default command for dune-watch."
  :type 'string
  :group 'dune-watch)

(defcustom dune-watch-command-format
  "opam exec -- dune %s --watch"
  "Format of command to run to invoke dune watch mode."
  :type 'string
  :group 'dune-watch)

(defcustom dune-watch-delete-buffer-on-termination t
  "Whether to delete the dune watch buffer when the dune watch process terminates."
  :type 'bool
  :group 'dune-watch)

(defcustom dune-watch-popup-function #'display-buffer-pop-up-window
  "Function passed to `display-buffer' to present compilation output to user."
  :type 'function
  :group 'dune-watch)

(defcustom dune-watch-read-command t
  "Whether the user should be prompted to select a build task."
  :type 'bool
  :group 'dune-watch)

;;;; Constants 

(defconst dune-watch-buffer-name "*dune-watch*"
  "Name of buffer used to track buffer outputs.")

(defconst dune-watch-process-name "*dune-watch-process*"
  "Name of process used to run dune watch.")

(defconst dune-watch-header "********** NEW BUILD"
  "Prefix of the header of dune watch output.")

(defconst dune-watch-read-prompt "dune task (default: %s): "
  "Prompt displayed to the user to dune task.")

(defconst dune-watch-supported-commands '("build" "test")
  "List of dune build tasks supported by dune-watch.")

;;;; Global variables 

(defvar dune-watch-task-history nil
  "Variable used to track history of dune build tasks.")

;;;; Local variables 
(defvar-local dune-watch-buffer nil
  "Buffer corresponding to dune watch for current program")

(defvar-local dune-watch-process nil
  "Process corresponding to dune watch for current program")

(defvar-local dune-watch-header-start nil
  "Start of the header of the most recent dune watch output.")

;;;; Implementation 

(defun dune-watch-generate-new-buffer ()
  "Return a new buffer to be used by dune watch."
  (let ((buffer (generate-new-buffer dune-watch-buffer-name)))
    (with-current-buffer buffer
      (compilation-mode)
      (setq dune-watch-buffer buffer)
      (setq dune-watch-process nil)
      (setq dune-watch-header-start nil))
    buffer))

(defun dune-watch-sentinel-function (watch-buffer process event)
  "Process sentinel used by dune-watch-minor-mode.

WATCH-BUFFER is the buffer used by dune watch.
PROCESS is the dune-watch process name
EVENT is the text output by the sentinel."
  (when (and dune-watch-delete-buffer-on-termination (buffer-live-p watch-buffer))
    (kill-buffer watch-buffer))
  (message "Dune watch process %s terminated with message \"%s\"" process (string-trim event)))

(defun dune-watch-update-header-start ()
  "Update the position of the header of the most recent dune watch output."
  (save-excursion
    (goto-char (point-max))
    (setq dune-watch-header-start
          (search-backward dune-watch-header nil t))))

(defun dune-watch-beautify-buffer ()
  "Cleans dune watch buffer by removing all but the last build output."
  (let ((inhibit-read-only t)
        (buffer-start (point-min))) 
    (when (and dune-watch-header-start buffer-start)
      (delete-region buffer-start dune-watch-header-start)
      (setq dune-watch-header-start buffer-start))))

(defun dune-watch-contains-errors ()
  "Determines whether the dune watch buffer output contain any errors."
  (save-excursion
    (when dune-watch-header-start
      (goto-char dune-watch-header-start)
      (search-forward "File" nil t))))

(defun dune-watch-popup-buffer ()
  "Pops up compilation output to user."
  (when dune-watch-buffer
    (display-buffer dune-watch-buffer dune-watch-popup-function)))

(defun dune-watch-filter-function (watch-buffer process event)
  "Process filter function used by dune watch.

WATCH-BUFFER is the buffer corresponding to the process.
PROCESS is the name of the process.
EVENT is the string returned by the dune watch."
  (when (and watch-buffer (buffer-live-p watch-buffer))
    (with-current-buffer watch-buffer
      (let ((inhibit-read-only t)
            (buffer-end (point-max)))
        (goto-char buffer-end)
        (insert event)
        (dune-watch-update-header-start)
        (dune-watch-beautify-buffer)
        
        (when (dune-watch-contains-errors)
            (dune-watch-popup-buffer))))))

(defun dune-watch-on-kill-buffer ()
  "Kill dune-watch process when main buffer is killed."
  (when (and dune-watch-minor-mode dune-watch-process  (process-live-p dune-watch-process))
    (kill-process dune-watch-process)))

(defun start-dune-watch (command)
  "Start a subprocess to run the dune COMMAND using the --watch flag."
  (message "starting process to watch %s task..."  command)
  (let* ((buffer (dune-watch-generate-new-buffer))
         (filter-process (apply-partially #'dune-watch-filter-function buffer))
         (sentinel-process (apply-partially #'dune-watch-sentinel-function buffer))
         (command (format dune-watch-command-format command)))
    (setq dune-watch-buffer buffer)
    (setq dune-watch-process (make-process
                              :name dune-watch-process-name
                              :buffer buffer
                              :command (split-string command)
                              :filter filter-process
                              :sentinel sentinel-process))
    (add-hook 'kill-buffer-hook #'dune-watch-on-kill-buffer)))

(define-minor-mode dune-watch-minor-mode "A minor mode to run dune commands"
  :group 'dune-watch
  :init-value nil
  (let ((command dune-watch-default-command))
    (when dune-watch-read-command
      (setq command (or (completing-read
                         (format dune-watch-read-prompt dune-watch-default-command)
                         dune-watch-supported-commands
                         nil nil dune-watch-default-command 'dune-watch-task-history)
                        command)))
    (start-dune-watch command)))

(provide 'dune-watch)
;;; dune-watch.el ends here
