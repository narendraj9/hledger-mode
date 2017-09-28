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

(require 'cl-lib)
(require 'popup)

(require 'hledger-core)

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

(defvar hledger-display-percentages nil
  "Variable accompanying `hledger-display-percentags' to maintain state.")

(defun hledger-remove-overlays ()
  "Remove overlays from beg to end in `hledger-display-percentages'."
  (remove-hook 'post-command-hook #'hledger-remove-overlays)
  (remove-overlays (get 'hledger-display-percentages 'beg)
                   ;; Take into account the margin and just one more
                   ;; char
                   (+ 2 (get 'hledger-display-percentages 'end))))

(defun hledger-remove-overlays-hook ()
  "Hook to be called for removing overlays created for % display."
  (remove-hook 'post-command-hook #'hledger-remove-overlays-hook)
  (add-hook 'post-command-hook #'hledger-remove-overlays))

(defun hledger-find-balance-delimits ()
  "Return the beginning and end point positions for shown --flat bals.
Returns a cons pair of the point values.  Returns nil if there is
not balance at point."
  (let* ((beg (save-excursion
                (forward-line 0)
                (while (and (looking-at hledger-whitespace-amount-regex)
                            (not (bobp)))
                  (forward-line -1))
                (if (not (looking-at hledger-whitespace-amount-regex))
                    (forward-line))
                (point)))
         (end (save-excursion
                (forward-line 0)
                (while (and (looking-at hledger-whitespace-amount-regex)
                            (not (eobp)))
                  (forward-line))
                (if (not (looking-at hledger-whitespace-amount-regex))
                    (forward-line -1))
                (end-of-line)
                (point))))
    (when (< beg end)
      (cons beg end))))

(defun hledger-display-percentages ()
  "Display percentages for the balances around the point."
  (interactive)
  (make-local-variable 'hledger-display-percentages)
  (let* ((amounts-with-delims-in-col (hledger-amounts-in-column))
         ;; Delimits for flat account names, i.e lines starting with
         ;; amount.
         (flat-delims (hledger-find-balance-delimits))
         ;; Prefer flat amounts
         (beg-end (or flat-delims
                      (cdr amounts-with-delims-in-col)))
         (beg (and beg-end (car beg-end)))
         (end (and beg-end (cdr beg-end)))
         (amounts (if flat-delims '() (car amounts-with-delims-in-col)))
         ;; Display overlay starting this column if flat-delims is nil
         (overlay-column (save-excursion (and end
                                              (goto-char end)
                                              ;; 1+ is the margin value
                                              (1+ (current-column))))))
    (when (and beg end)
      (save-excursion
        ;; Collect amounts only when we are looking at flat account
        ;; names with balances as in income statement.
        (when flat-delims
          (goto-char end)
          (while (re-search-backward hledger-amount-regex beg t)
            (push (string-to-number (replace-regexp-in-string
                                     hledger-currency-string
                                     ""
                                     (match-string 0)))
                  amounts)))
        ;; Now that we have the amounts. Let's create overlays.
        (goto-char beg)
        (let* ((pos-amounts (seq-filter (lambda (n)
                                          (< 0 n))
                                        amounts))
               (neg-amounts (seq-filter (lambda (n)
                                          (not (< 0 n)))
                                        amounts))
               (pos-amounts-sum (cl-reduce '+ pos-amounts :initial-value 0.0))
               (neg-amounts-sum (cl-reduce '+ neg-amounts :initial-value 0.0))
               (hledger-pchart-format
                (concat "%-"
                        (number-to-string hledger-percentage-chart-width)
                        "s")))
          (dolist (amount amounts)
            ;; Overlay for display the percentage
            (let ((amounts-sum (if (< 0 amount)
                                   pos-amounts-sum
                                 neg-amounts-sum)))
              ;; Add overlay column if it's just a column of amounts.
              (overlay-put (make-overlay (+ (line-beginning-position)
                                            (if flat-delims 0 overlay-column))
                                         (+ (line-beginning-position)
                                            (if flat-delims 0 overlay-column)))
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
                             'font-lock-face hledger-percentage-chart-face))))

            (forward-line))))
      (setq hledger-display-percentages t)
      (put 'hledger-display-percentages 'beg beg)
      (put 'hledger-display-percentages 'end end)
      (add-hook 'post-command-hook 'hledger-remove-overlays-hook))))


(defun hledger-sort-flat-balances (prefix)
  "Sort the flat balances according the amount value.
This assumes that the amount value appears in the second column
after the currency sign.  So, it won't work for different
commodities with differently positioned commodity signs.
Argument PREFIX is the universal argument to decide whether to
reverse the direction of sorting."
  (interactive "P")
  (let* ((inhibit-read-only t)
         (beg-end (hledger-find-balance-delimits))
         (beg (car beg-end))
         (end (cdr beg-end)))
    (sort-numeric-fields 2 beg end)
    (if (not prefix)
        (reverse-region beg end))))





(defun hledger-amounts-in-column ()
  "Return a sequence of consecutive amounts in current column.
Returns a cons cell with amounts and the delimiting point
values."
  (let ((col (current-column))
        (amounts '())
        (end nil)
        (beg nil))
    (when (thing-at-point 'hledger-amount)
      (save-excursion
        ;; Let's go all the way down first
        (while (and (thing-at-point 'hledger-amount)
                    (not (eobp)))
          (forward-line)
          (move-to-column col))
        ;; Move to the last amount
        (forward-line -1)
        (move-to-column col)
        ;; Store the first end point of the amount
        (setq end (cdr (bounds-of-thing-at-point 'hledger-amount)))
        ;; Start collection amounts now
        (while (and (thing-at-point 'hledger-amount)
                    (not (bobp)))
          (push (string-to-number (thing-at-point 'hledger-amount))
                amounts)
          (forward-line -1)
          (move-to-column col))
        ;; Move to the first amount
        (forward-line)
        (move-to-column col)
        ;; Store the last end point of the amount
        (setq beg (car (bounds-of-thing-at-point 'hledger-amount)))

        ;; Returns (amounts . (beg . end))
        (cons amounts
              (cons beg end))))))

(defun hledger-group-digits (number)
  "Group the digits of NUMBER to make it more readable.
Returns a string with commas added appropriately.

Note: I am not handling the edge cases here.  It's okay if the number
looks ugly when it's small."
  (let* ((number-string (number-to-string number))
         (number-hundreds-and-rest (mod number 1000))
         (number-but-hundreds-and-rest (/ number 1000))
         (number-tail-string (format "%03d" number-hundreds-and-rest))
         (number-head-string (if (= 0 number-but-hundreds-and-rest)
                                 ""
                               (number-to-string number-but-hundreds-and-rest)))
         (number-head-pairs (mapcar 'reverse
                                    (reverse (seq-partition
                                              (reverse number-head-string)
                                              2))))
         (number-head-triplets (mapcar 'reverse
                                       (reverse (seq-partition
                                                 (reverse number-head-string)
                                                 3))))
         (number-english-format (concat
                                 (mapconcat 'identity number-head-triplets ",")
                                 (if number-head-triplets "," "")
                                 number-tail-string))
         (number-hindi-format (concat
                               (mapconcat 'identity number-head-pairs ",")
                               (if number-head-triplets "," "")
                               number-tail-string)))
    ;; Assuming `hledger-currency-string' is already defined.
    (if (and (boundp 'hledger-currency-string)
             (equal hledger-currency-string "₹"))
        number-hindi-format
      number-english-format)))

(defun hledger-humanize-float-months (n)
  "Convert a float value N months into a proper human readable string."
  (let* ((whole-part (truncate n))
         (decimal-part (- n whole-part))
         (years (/ whole-part 12))
         (months (mod whole-part 12))
         (days (truncate (* 30 decimal-part))))
    (if (= years days months 0)
        "0 days"
      (mapconcat 'identity
                 (seq-filter
                  (lambda (s) (not (equal s "")))
                  (list (if (< 0 years) (format "%d year%s"
                                                years
                                                (if (< 1 years) "s" ""))
                          "")
                        (if (< 0 months) (format "%d month%s"
                                                 months
                                                 (if (< 1 months) "s" ""))
                          "")
                        (if (< 0 days) (format "%s%d day%s"
                                               (if (or (< 0 years) (< 0 months))
                                                   "and "
                                                 "")
                                               days
                                               (if (< 1 days) "s" ""))
                          "")))
                 " "))))

(defun hledger-completion-at-point ()
  "Adding this for account name completions in `minibuffer'."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (start (car bounds))
         (end (cdr bounds)))
    (list start end hledger-accounts-cache . nil)))


(provide 'hledger-defuns)
;;; hledger-defuns.el ends here
