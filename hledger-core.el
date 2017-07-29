;;; hledger-core.el --- Core major mode facilities   -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords: convenience, local

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

;;; Code:

(defcustom hledger-jfile "~/.hledger.journal"
  "Location of the journal file."
  :group 'hledger
  :type 'file)

(defcustom hledger-reporting-buffer-name "*Personal Finance*"
  "Name of the buffer for showing or working with reports."
  :group 'hledger
  :type 'string)

(defcustom hledger-comments-column 11
  "Column number where the comments start."
  :group 'hledger
  :type 'integer)

(defcustom hledger-currency-string "₹"
  "String to be used for currency.  Assumes it is prefixed."
  :group 'hledger
  :type 'integer)

;;; Regexes
(defvar hledger-empty-regex "^\\s-*$"
  "Regular expression for an empty line.")
(defvar hledger-date-only-regex "^\\s-*[0-9]\\{4\\}[-/][0-9]\\{2\\}[-/][0-9]\\{2\\}\\s-*$"
  "Regular expression a line with date only.")
(defvar hledger-date-regex "[0-9]\\{4\\}[-/][0-9]\\{2\\}[-/][0-9]\\{2\\}"
  "Regular expression for dates for font lock.")
(defvar hledger-date-and-desc-regex (format "\\<%s\\s-*\\*?\\s-*[^[:space:]]+\\>" hledger-date-regex)
  "Regular expression for matching a starting entry with some description.")
(defvar hledger-account-regex "\\(\\([Rr]evenues\\|[aA]ssets\\|[lL]iabilities\\|[Ee]quity\\|[Ee]xpenses\\|[iI]ncome\\|[Zz]adjustments\\)\\(:[A-Za-z--]+\\)+\\)"
  "Regular expression for a potential journal account.")
(defvar hledger-whitespace-account-regex (format "\\s-*%s" hledger-account-regex)
  "Regular expression for an account with leading whitespace.")
(defvar hledger-comment-regex "^[ \t]*;"
  "Regular expression for a comment in journal file.")
(defvar hledger-empty-comment-regex "^\\s-*;\\s-*$"
  "Regular expression to match a comment with no text.")
(defvar hledger-amount-value-regex "[-]?[0-9]+\\(\\.[0-9]+\\)?"
  "Regular expression to match a floating point number.")
(defvar hledger-amount-regex (format "\\<%s\\s-*[-]?[0-9]+\\(\\.[0-9]+\\)?\\>"
                                     hledger-currency-string)
  "Regular expression to match an inserted amount in rupees.")
(defvar hledger-whitespace-amount-regex (format "\\s-*%s"
                                                hledger-amount-regex)
  "Regular expression for whitespace followed by amount.")

;;; Indentation
(defun hledger-line-matchesp (re offset)
  "Check if regex RE will match the beginning for line current-line - OFFSET."
  (save-excursion
    (forward-line offset)
    (beginning-of-line)
    (looking-at re)))

;; Internal functions for looking-at lines' beginnings
(defun hledger-cur-line-matchesp (re)
  "Return true if current line has regex RE in the beginning."
  (hledger-line-matchesp re 0))
(defun hledger-prev-line-matchesp (re)
  "Return true if previous line has regex RE in the beginning."
  (hledger-line-matchesp re -1))

;; Auxiliary funtions[s]
(defun hledger-delete-cur-line ()
  "Delete the current line."
  (delete-region (line-beginning-position) (line-end-position)))
(defun hledger-insert-date ()
  "Insert date at point."
  (insert (format-time-string "%Y-%m-%d ")))
(defun hledger-insert-comment ()
  "Insert a comment on the current line."
  (indent-line-to hledger-comments-column)
  (insert "; "))
(defun hledger-insert-rupee ()
  "Insert the amount for a transaction in hledger."
  (beginning-of-line)
  (re-search-forward hledger-whitespace-account-regex)
  (insert (concat "   " hledger-currency-string " ")))
(defun hledger-delete-rupee-sign ()
  "Delete the rupee sign."
  (beginning-of-line)
  (re-search-forward hledger-whitespace-account-regex
                     (line-end-position)
                     t)
  (delete-region (point) (line-end-position)))

(defun hledger-acc-line-has-rupeep ()
  "Return true if the account line has an amount."
  (hledger-cur-line-matchesp (concat hledger-whitespace-account-regex
                                     (format "\\s-*%s\\s-*$"
                                             hledger-currency-string))))
(defun hledger-expecting-rupeep ()
  "Return true if we should insert a rupee sign."
  (hledger-cur-line-matchesp (concat hledger-whitespace-account-regex
                                     "\\s-*$")))

(defun hledger-cur-line-emptyp ()
  "Return true if current line is empty."
  (hledger-cur-line-matchesp hledger-empty-regex))
(defun hledger-cur-has-datep ()
  "Return true if current line has only date."
  (hledger-cur-line-matchesp hledger-date-only-regex))
(defun hledger-cur-has-date-and-descp ()
    "Return tru if current line had date and description."
  (hledger-cur-line-matchesp hledger-date-and-desc-regex))
(defun hledger-cur-has-empty-commentp ()
  "Return true if current line has an empty comment.  Empty comments."
  (hledger-cur-line-matchesp hledger-empty-comment-regex))
(defun hledger-cur-has-accp ()
  "Return true if the current line has an account name."
  (hledger-cur-line-matchesp hledger-whitespace-account-regex))
(defun hledger-cur-starts-with-semicolp ()
  "Return true if the current line has a semicolon in the beginning."
  (hledger-cur-line-matchesp hledger-comment-regex))

(defun hledger-prev-line-emptyp ()
  "Return true if previous line is empty."
  (hledger-prev-line-matchesp hledger-empty-regex))
(defun hledger-prev-has-datep ()
  "Return true if previous line has date and description."
  (hledger-prev-line-matchesp hledger-date-and-desc-regex))
(defun hledger-prev-has-commentp ()
  "Return true if previousl line has an empty comment.  Empty or otherwise."
  (hledger-prev-line-matchesp hledger-comment-regex))
(defun hledger-prev-has-accp ()
  "Return true if the previous line has an account name."
  (hledger-prev-line-matchesp hledger-whitespace-account-regex))

(defun hledger-indent-empty-line ()
  "Called when the line to be indented is an empty one."
  (cond
   ((hledger-prev-line-emptyp)   (hledger-insert-date))
   ((hledger-prev-has-datep) (if (= (current-indentation) tab-width)
                                 (hledger-insert-comment)
                               (hledger-delete-cur-line)
                               (indent-line-to tab-width)))
   ((hledger-prev-has-commentp) (hledger-insert-comment))
   ((hledger-prev-has-accp)
    (indent-line-to tab-width))))

(defun hledger-indent-date-line ()
  "Called when current line has only a date in the beginning."
  (hledger-delete-cur-line))

(defun hledger-indent-comment-line ()
  "Called when current line has an empty comment already."
  (if (not (hledger-cur-has-empty-commentp))
      (indent-line-to hledger-comments-column)
    (hledger-delete-cur-line)
    (indent-line-to tab-width)))

(defun hledger-indent-account-line ()
  "Called when the line to indent is an account listing line."
  (cond
   ((hledger-acc-line-has-rupeep) (hledger-delete-rupee-sign))
   ((hledger-expecting-rupeep) (hledger-insert-rupee))
   (t (indent-line-to tab-width))))

(defun hledger-indent-line ()
  "Indent the current line."
  (cond
   ((hledger-cur-line-emptyp) (hledger-indent-empty-line))
   ((hledger-cur-has-datep) (hledger-indent-date-line))
   ((hledger-cur-starts-with-semicolp) (hledger-indent-comment-line))
   ((hledger-cur-has-accp) (hledger-indent-account-line))))

(defun hledger-indent-region-function (start end)
  "Indent region (START END) according to `hledger-mode'.
We need a separate function because we do different stuff while
interactively editing an entry."
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (beginning-of-line)
      (cond
       ((hledger-cur-has-datep) (indent-line-to 0))
       ((hledger-cur-starts-with-semicolp) (indent-line-to hledger-comments-column))
       ((hledger-cur-has-accp) (indent-line-to tab-width)))
      (forward-line 1))))

(provide 'hledger-core)
;;; hledger-core.el ends here
