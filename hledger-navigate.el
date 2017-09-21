;;; hledger-navigate.el --- Functions for navigating around an hledger buffer.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords: data, convenience

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

;;
;; Functions related to navigating an hledger journal file containing
;; buffer.  This file would also have functions for mutating a buffer,
;; e.g. for changing the date.


;;; Code:

(require 'hledger-core)
(require 'pulse)

(defvar hledger-jentry-hook nil
  "Hook for `hledger-jentry'.")

;; Things in hledger
(defvar hledger-amount 0
  "Variable to be used for looking at amount at point.")
(defvar hledger-date "18-06-1993"
  "Variable to be used for looking at date at point.")
(defvar hledger-account "assets"
  "Variable to be used for looking at account name at point.")

(defcustom hledger-enable-current-overlay nil
  "Boolean to decide whether to enable current entry overlay."
  :group 'hledger
  :type 'boolean)

(defcustom hledger-current-entry-overlay-face
  '(:background "dark slate grey" :height 1.1)
  "Face for the current journal entry's overlay."
  :group 'hledger
  :type 'face)

(defvar hledger-current-entry-beg nil
  "Variable to store the (point) for beginning of current entry.")
(make-variable-buffer-local 'hledger-current-entry-beg)

(defvar hledger-current-entry-end nil
  "Variable to store the (point) for end of current entry.")
(make-variable-buffer-local 'hledger-current-entry-end)

(defvar hledger-current-entry-overlay nil
  "Overlay that spans the currently journal entry.")

(defun hledger-pulse-momentary-current-entry ()
  "Pulse highlight journal entry at point."
  (save-excursion
    (pulse-momentary-highlight-region
     (if (looking-at hledger-date-regex)
         (line-beginning-position)
       (or (hledger-backward-entry)
           (point-min)))
     (or (hledger-forward-entry) (point-max))
     'next-error)))

(defun hledger-ret-command ()
  "Commands run on <return> in ‘hledger-mode’."
  (interactive)
  (newline-and-indent))

(defun hledger-backtab-command ()
  "Commands runon <backtab> in ‘hledger-mode’."
  (interactive)
  (backward-delete-char-untabify tab-width))

(defun hledger-kill-reporting-window ()
  "Kill the reporting buffer and window."
  (interactive)
  (if (>= (length (window-list)) 2)
      (kill-buffer-and-window)
    (kill-buffer)))

(defun hledger-reschedule ()
  "Reschedule the transaction at point.
Note: This function uses `org-read-date'."
  (interactive)
  (save-excursion
    (let ((new-date (org-read-date)))
      (forward-line 0)
      (when (not (looking-at hledger-date-regex))
        (search-backward-regexp hledger-date-regex))
      ;; Erase the old date
      (delete-region (line-beginning-position)
                     (search-forward-regexp hledger-date-regex))
      ;; Insert the new date
      (insert new-date)
      (pulse-momentary-highlight-region (line-beginning-position)
                                        (line-end-position)))))

(defun hledger-go-to-starting-line ()
  "Function to go the first line that stars a new entry.  Cleans up whitespace."
  (goto-char (point-max))
  (beginning-of-line)
  (while (and (looking-at hledger-empty-regex)
              (not (bobp)))
    (forward-line -1))
  (end-of-line)
  (let ((times-yet-to-move (forward-line 2)))
    (insert (make-string times-yet-to-move ?\n))))

(defun hledger-jentry ()
  "Make a new entry in the financial journal.  Avoids editing old entries."
  (interactive)
  (find-file hledger-jfile)
  (hledger-go-to-starting-line)
  (run-hooks 'hledger-jentry-hook)
  (recenter))

(defun hledger-forward-entry (&optional n)
  "Move past N hledger entries.
With a prefix argument, repeat that many times.
Returns nil if we have reached the end of the journal."
  (interactive "p")
  ;; To make sure we are not on the date of the first entry.
  (end-of-line)
  (let ((p (search-forward-regexp hledger-date-regex nil t (or n 1))))
    (forward-line 0)
    (and p (point))))

(defun hledger-next-or-new-entry (&optional n)
  "Move to the next entry or the beginning of a new one.
Argument N is passed onto `hledger-forward-entry'."
  (interactive "p")
  (or (hledger-forward-entry n)
      (hledger-go-to-starting-line)))

(defun hledger-backward-entry (&optional n)
  "Move backward by N hledger entries.
With a prefix argument, repeat that many times.
Returns nil if we are at the beginning of the journal."
  (interactive "p")
  ;; To make sure we skip the current entry.
  (forward-line 0)
  (when (search-backward-regexp hledger-date-regex nil t (or n 1))))

(defun hledger-bounds-of-thing-at-point (thing-regexp &optional sep-regexp)
  "Return the (beg . end) point positions for amount at point.
To make this work, one must be either inside or after thing at point in buffer.
Argument THING-REGEXP is the regular expression that matches the thing.
Optional argument SEP-REGEXP is the regular expression that separates things."
  (let* ((here (point))
         ;; Search back for separator
         (beg (progn
                (when (search-backward-regexp (or sep-regexp
                                                  "\\s-+")
                                              (point-min)
                                              t)
                  (search-forward-regexp (or sep-regexp
                                             "\\s-+")
                                         here
                                         t))))
         ;; Search forward for separator
         (end-bound (save-excursion
                      (search-forward-regexp (or sep-regexp
                                                 "\\s-+")
                                             (point-max)
                                             t)))
         ;; Search for the thing starting the first separator ^
         (end (search-forward-regexp thing-regexp
                                     (or end-bound
                                         (point-max))
                                     t)))
    ;; Restore point
    (goto-char here)
    ;; If any one of the ends is nil, return nil.
    (and (and beg end)
         (cons beg end))))

(defun hledger-bounds-of-current-entry ()
  "Return the bounds of the current journal entry."
  (save-excursion
    (let* ((x (hledger-forward-entry))
           (y (hledger-backward-entry))
           (new-bounds (cond
                        ((and x y) (cons y x))
                        ;; We are at the last entry of the journal.
                        ;; Either there is a previous entry or there isn't.
                        ((and y (not x)) (cons (or (hledger-forward-entry)
                                                   (point))
                                               (point-max)))
                        ;; We are at the first entry of the journal.
                        ;; Will these ever be reached?
                        ((not y) (cons x (hledger-forward-entry)))))
           (new-x (car new-bounds)))
      ;; Skip empty lines from the overlay
      (goto-char (cdr new-bounds))
      (while (or (looking-at hledger-empty-regex)
                 (looking-at hledger-date-regex))
        (forward-line -1))
      (cons new-x (line-end-position)))))

(defun hledger-bounds-of-account-at-point ()
    "Return the bounds of an account name at point."
  (hledger-bounds-of-thing-at-point hledger-account-regex))

(defun hledger-bounds-of-date-at-point ()
    "Return the bounds of date at point."
  (hledger-bounds-of-thing-at-point hledger-date-regex))

(defun hledger-bounds-of-amount-at-point ()
  "Return the bounds of a floating point number at point."
  (hledger-bounds-of-thing-at-point hledger-amount-value-regex))

(defun hledger-init-thing-at-point ()
    "Setup properties for thingatpt.el."
  (put 'hledger-account
       'bounds-of-thing-at-point
       'hledger-bounds-of-account-at-point)
  (put 'hledger-amount
       'bounds-of-thing-at-point
       'hledger-bounds-of-amount-at-point)
  (put 'hledger-date
       'bounds-of-thing-at-point
       'hledger-bounds-of-date-at-point))

(defun hledger-update-current-entry-overlay ()
  "Update the overlay for the current journal entry."
  ;; Only run this in a `hledger-mode' buffer. For example, M-x
  ;; command would cause this to fail otherwise.
  (when (eq major-mode 'hledger-mode)
    ;; Initialize if required.
    (unless hledger-current-entry-overlay
      (setq hledger-current-entry-overlay
            (make-overlay (point-max) (point-max) (current-buffer) t t))
      (overlay-put hledger-current-entry-overlay
                   'face hledger-current-entry-overlay-face))
    ;; Now let's update the overlay.
    (if (and hledger-current-entry-beg
             hledger-current-entry-end
             (and (<= hledger-current-entry-beg (point))
                  (< (point) hledger-current-entry-end)))
        nil
      (let* ((bounds-of-entry (hledger-bounds-of-current-entry)))
        (setq hledger-current-entry-beg (car bounds-of-entry))
        (setq hledger-current-entry-end (cdr bounds-of-entry))
        (move-overlay hledger-current-entry-overlay
                      hledger-current-entry-beg
                      hledger-current-entry-end)
        (overlay-put hledger-current-entry-overlay
                     'after-string
                     (propertize " "
                                 'display
                                 `((space :align-to ,(window-text-width)))
                                 'face hledger-current-entry-overlay-face
                                 'cursor t))))))

(defun hledger-toggle-star ()
  "Toggle the star status of a journal entry."
  (interactive)
  (save-excursion
    (let ((there (line-end-position)))
      (beginning-of-line)
      (while (not (looking-at hledger-date-and-desc-regex))
        (forward-line -1))
      ;; Update the date to today after each toggle
      (search-forward-regexp hledger-date-regex nil t)
      (delete-region (line-beginning-position) (point))
      (hledger-insert-date)
      ;; Now handle the start/unstar stuff
      (if (search-forward "*" there t)
          (delete-char -3)
        (insert "*")))))

(defun hledger-op-on-amount (op)
  "Apply operation OP on the previous amount in sight."
  (save-excursion
    (if (search-forward-regexp hledger-amount-regex nil t)
        (let* ((amount-bounds (bounds-of-thing-at-point 'hledger-amount))
               (amount (string-to-number (thing-at-point 'hledger-amount)))
               (beg (car amount-bounds))
               (end (cdr amount-bounds))
               (new-amount (funcall op amount)))
          (delete-region beg end)
          (insert (format "%s" new-amount))
          (pulse-momentary-highlight-region beg end))
      (message "No journal entry after point."))))

(defun hledger-increment-amount (&optional p)
  "Increment amount by 1.
With prefix argument P, increment by that number."
  (interactive "p")
  (hledger-op-on-amount (lambda (amount)
                          (+ amount (or p 1)))))

(defun hledger-edit-amount (amount)
  "Update the previous amount in the buffer with AMOUNT."
  (interactive "nAmount: ")
  (hledger-op-on-amount (lambda (_) amount)))

(provide 'hledger-navigate)
;;; hledger-navigate.el ends here
