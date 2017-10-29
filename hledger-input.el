;;; hledger-input.el --- Facilities for entering journal entries conveniently  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords: data

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains functions that let you enter a journal entry
;; quickly without opening the journal file.  It also let's you
;; quickly view reports after submitting a new entry.  The idea is to
;; be able to create common workflows that people follow everyday.

;;; Code:
(require 'hledger-core)
(require 'hledger-mode)
(require 'hledger-reports)

(defcustom hledger-input-buffer-height 10
  "Number of lines to show in the hledger input buffer."
  :group 'hledger
  :type 'numebr)

(defvar hledger-input-pre-commit-hook nil
  "Hook run in the input buffer before a commit to `hledger-jfile'.")

(defvar hledger-input-post-commit-hook nil
  "Hook run after commit to `hledger-file' before closing the input buffer.
A useful function for this would be `hledger-show-new-balances'.")

(defvar hledger-input-mode-map
  (let ((map (copy-keymap hledger-mode-map)))
    (define-key map (kbd "C-c C-c") 'hledger-commit-input)
    (define-key map (kbd "C-c C-k") 'hledger-discard-input)
    (define-key map (kbd "C-c e") 'hledger-discard-input-jentry)
    map)
  "Keymap for hledger input buffers.")

(defun hledger-create-input-buffer ()
  "Create and return a buffer in `hledger-mode' for a journal entry.
This setups up the minor mode and narrowing in the input buffer."
  (let* ((input-buffer (get-buffer-create "*Journal Entry*")))
    (with-current-buffer input-buffer
      ;; No auto saving in this buffer as we want to commit when we
      ;; like.
      (auto-save-mode -1)
      (hledger-input-mode +1)
      input-buffer)))

(defun hledger-get-accounts-in-buffer ()
  "Return a sequence of accounts currently in buffer."
  (let ((result '()))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp hledger-account-regex
                                    nil
                                    t)
        (push (substring-no-properties (thing-at-point 'hledger-account))
              result)))
    result))

(defun hledger-show-new-balances ()
  "Show balances new balances for the accounts in buffer."
  (let* ((inhibit-read-only t)
         (message-log-max nil)
         (accounts (hledger-get-accounts-in-buffer))
         (report-str* (hledger-get-balances accounts))
         (report-str (format "%s\n\n%s\n%s"
                             (make-string 20 ?―)
                             report-str*
                             (make-string 20 ?―))))
    (display-message-or-buffer report-str)
    nil))

(defun hledger-input-valid-p (input-entry)
  "Check the validity of balances in INPUT-ENTRY."
  (let* ((temp-file-path (make-temp-file "hledger-input"))
         (hledger-jfile temp-file-path))
    (with-temp-file temp-file-path
      (insert input-entry)
      (insert "\n"))

    (let ((result (hledger-status "balance")))
      (delete-file temp-file-path)
      (if (not (consp result))
          t
        (message "Error: \n%s\n%s\n%s"
                 (make-string fill-column ?-)
                 (cdr result)
                 (make-string fill-column ?-))
        nil))))

(defun hledger-commit-input ()
  "Commit INPUT-BUFFER contents to `hledger-jfile'.
We are already in the input-buffer."
  (interactive)
  (run-hooks 'hledger-input-pre-commit-hook)
  (let ((new-input (buffer-substring (point-min)
                                     (point-max))))
    (when (hledger-input-valid-p new-input)
      (with-current-buffer (find-file-noselect hledger-jfile)
        (hledger-go-to-starting-line)
        (insert new-input)
        (save-buffer)
        (kill-buffer))
      (message "Saved input to journal file")
      (run-hooks 'hledger-input-post-commit-hook)
      (kill-buffer)
      ;; Delete the window if it's not the sole window.
      (ignore-errors (delete-window)))))

(defun hledger-discard-input ()
  "Discard entry in input-buffer and go back to previous window configuration."
  (interactive)
  (kill-buffer)
  (delete-window))

(defun hledger-discard-input-jentry ()
  "Discard the current input buffer and do jentry."
  (interactive)
  (hledger-discard-input)
  (hledger-jentry))

(defun hledger-capture ()
  "Capture a journal entry quickly."
  (interactive)
  (select-window (split-window-below (- hledger-input-buffer-height)) )
  (switch-to-buffer (hledger-create-input-buffer))
  (hledger-input-mode))

(defun hledger-dispatch-command ()
  "Dispatch to a specific hledger REPORT."
  (interactive)
  (kill-buffer))

(define-minor-mode hledger-input-mode
  "A mode for quickly entering journal entries."
  :group 'hledger
  (hledger-mode)
  (use-local-map hledger-input-mode-map))

(provide 'hledger-input)
;;; hledger-input.el ends here
