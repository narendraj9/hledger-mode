;;; hledger-defuns.el --- Helper functions for hledger-mode.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords: convenience

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

;; This file contains code for functions defined to make
;; hledger-mode.el simpler and shorter.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'popup)

(defcustom hledger-display-percentage-face
  '(:foreground "Cornsilk" :background "DarkSlateGray")
  "Face for showing the percentage of a set of balances around point."
  :group 'hledger
  :type 'face)

(defcustom hledger-percentage-chart-face
  '(:foreground "Cornsilk" :background "DarkSlateGray")
  "Face for showing the percentage chart."
  :group 'hledger
  :type 'face)

(defcustom hledger-percentage-chart-char
  ?█
  "Character to use for drawing the percentage chart."
  :group 'hledger
  :type 'char)

(defcustom hledger-show-percentage-chart
  t
  "Boolean to decide if we show the chart alongside percentages."
  :group 'hledger
  :type 'boolean)

(defcustom hledger-percentage-chart-width
  20
  "Width of the percentage chart.")

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

(defun hledger-copy-to-clipboard ()
  "Copies current buffer contents to clipboard."
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max))
  (message "Buffer copied to clipboard"))

(defun hledger-append-clipboard-to-journal ()
  "Append clipboard contents to journal file."
  (interactive)
  (let ((entries (buffer-string)))
    (hledger-jentry)
    (insert entries)
    (format "Fetched entries appended.")))

(defmacro hledger-as-command (name command)
  "Define a function named NAME for hledger COMMAND."
  `(defun ,(intern (symbol-name name)) () (interactive)
          (setq hledger-last-run-time 0)
          (hledger-run-command ,command)
          (goto-char (point-min))))

(defun hledger-show-view-mode-help ()
  "Show help in hledger view mode."
  (interactive)
  (let ((result ""))
    ;; Show key binding for single character keys
    (map-keymap (lambda (k v)
                  (when (and (characterp k)
                             (symbolp v))
                    (setq result
                          (concat result
                                  (format "%c %s\n"
                                          k
                                          (replace-regexp-in-string "hledger-"
                                                                    ""
                                                                    (symbol-name v)))))))
                (current-local-map))
    (popup-tip result :margin t)))

(defun hledger-move-line (count)
  "Move COUNT lines skipping all empty lines."
  (forward-line count)
  (while (and (or (not (looking-at (concat hledger-whitespace-account-regex
                                           "\\|"
                                           hledger-whitespace-amount-regex)))
                  (looking-at hledger-empty-regex))
              (not (or (bobp) (eobp))))
    (forward-line (if (> count 0) 1 -1))))

(defun hledger-next-line ()
  "Move to next line.  See `hledger-move-line'."
  (interactive)
  (hledger-move-line 1))

(defun hledger-prev-line ()
  "Move to previous line.  See `hledger-move-line'."
  (interactive)
  (hledger-move-line -1))


(defun hledger-summarize ()
  "Show summary for the financial ratios."
  (interactive)
  (if hledger-ratios-summary
      (momentary-string-display hledger-ratios-summary
                                (if (equal hledger-last-run-command
                                           "overall")
                                    hledger-ratios-summary-point
                                  (point-max))
                                ?s
                                "Press 's' to hide")
    (message "Overall report hasn't been compiled yet.")))


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
      (insert new-date))))


(defvar hledger-display-percentages nil
  "Variable accompanying `hledger-display-percentags' to maintain state.")


(defun hledger-find-balance-delimits ()
  "Return the beginning and end point positions for shown --flat bals.
Returns a cons pair of the point values."
  (let* ((beg (save-excursion
                (while (and (looking-at hledger-whitespace-amount-regex)
                            (not (bobp)))
                  (forward-line -1))
                (if (not (looking-at hledger-whitespace-amount-regex))
                    (forward-line))
                (point)))
         (end (save-excursion
                (while (and (looking-at hledger-whitespace-amount-regex)
                            (not (eobp)))
                  (forward-line))
                (if (not (looking-at hledger-whitespace-amount-regex))
                    (forward-line -1))
                (end-of-line)
                (point))))
    (cons beg end)))


(defun hledger-display-percentages ()
  "Display percentages for the balances around the point."
  (interactive)
  (let* ((beg-end (hledger-find-balance-delimits))
         (beg (car beg-end))
         (end (cdr beg-end))
         (amounts '()))
    (if hledger-display-percentages
        (progn (remove-overlays beg (1+ end))
               (setq hledger-display-percentages nil))
      (save-excursion
        (goto-char end)
        (while (re-search-backward hledger-amount-regex beg t)
          (push (string-to-number (replace-regexp-in-string
                                   hledger-currency-string
                                   ""
                                   (match-string 0)))
                amounts))
        ;; Now that we have the amounts. Let's create overlays.
        (goto-char beg)
        (let ((amounts-sum (reduce '+ amounts))
              (hledger-pchart-format
               (concat "%-"
                       (number-to-string hledger-percentage-chart-width)
                       "s")))
          (dolist (amount amounts)
            ;; Overlay for display the percentage
            (overlay-put (make-overlay (line-beginning-position)
                                       (line-beginning-position))
                         'after-string
                         (concat
                          ;; Percentages
                          (propertize (format "  %5.2f%% "
                                              (* (/ amount amounts-sum)
                                                 100.0))
                                      'font-lock-face
                                      hledger-display-percentage-face)
                          ;; Percentage chart
                          (propertize
                           (if hledger-show-percentage-chart
                               (format hledger-pchart-format
                                       (make-string
                                        (round (* (/ amount amounts-sum)
                                                  hledger-percentage-chart-width))
                                        hledger-percentage-chart-char))
                             "")
                           'font-lock-face hledger-percentage-chart-face)))

            (forward-line))))
      (setq hledger-display-percentages t))))


(defun hledger-sort-flat-balances (prefix)
  "Sorts the flat balances according the amount value.
This assumes that the amount value appears in the second column
after the currency sign. So, it won't work for different
commodities with differently positioned commodity signs."
  (interactive "P")
  (let* ((inhibit-read-only t)
         (beg-end (hledger-find-balance-delimits))
         (beg (car beg-end))
         (end (cdr beg-end)))
    (sort-numeric-fields 2 beg end)
    (if (not prefix)
        (reverse-region beg end))))


(provide 'hledger-defuns)
;;; hledger-defuns.el ends here
