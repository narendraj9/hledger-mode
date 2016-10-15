;;; hledger-webservice.el --- Helper functions for hledger webservice  -*- lexical-binding: t; -*-

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

;; This file contains function definitions that would help me fetch
;; entries stored in the hledger webapp hosted at
;; https://services.vicarie.in

;;; Code:

(require 'hledger-core)
(require 'hledger-reports)
(require 'json)

(defcustom hledger-service-fetch-url
  "https://services.vicarie.in/api/entry"
  "Service url for fetching journal entries."
  :type 'string
  :group 'hledger)

(defun hledger-format-comment-string (comment)
  "Format the input COMMENT string for insertion into a journal file."
  (with-temp-buffer (progn
                      (if (string-match-p hledger-empty-regex comment)
                          ""
                        (electric-indent-mode -1)
                        (setq-local fill-column (- 70 hledger-comments-column))
                        (insert comment)
                        (insert "\n")
                        (goto-char (point-min))
                        (fill-paragraph)
                        (setq-local comment-start "; ")
                        (setq-local comment-end "")
                        (comment-region (point-min) (point-max))
                        (indent-region (point-min) (point-max) hledger-comments-column)
                        (buffer-string)))))


(defun hledger-fetch-entries-insert (entries)
  "Insert ENTRIES into a journal buffer."
  (let ((result ""))
    (dolist (entry (reverse entries))
      (let ((description (cdr (assoc 'description entry)))
            (comment (hledger-format-comment-string
                      (cdr (assoc 'comment entry))))
            (postings (cdr (assoc 'postings entry)))
            (date (cdr (assoc 'date entry))))
        (setf result
              (concat result
                      (format "%s %s\n%s"
                              date
                              description
                              comment)))
        (dolist (posting (append postings nil))
          (let ((account (cdr (assoc 'account posting)))
                (amount (cdr (assoc 'amount posting))))
            (setf result
                  (concat result
                          (format "    %s    %s %s\n"
                                  account
                                  (if (string-match "[0-9]+" amount)
                                      hledger-currency-string
                                    "" )
                                  amount))))))
      (setf result (concat result "\n")))
    (kill-buffer (current-buffer))
          
    (let ((jbuffer (hledger-get-perfin-buffer nil t)))
      (with-current-buffer jbuffer
        (insert result))
      (pop-to-buffer jbuffer)
      (goto-char (point-min)))))

(defun hledger-fetch-entries ()
  "Fetch journal entries from `hledger-service-url`.
Show the results in the `hledger-reporting-buffer-name' buffer.
**This is a workaround**."
  (interactive)
  (browse-url hledger-service-fetch-url)
  (read-from-minibuffer "Opening browser. Hit [Enter] after copy. ")
  (with-temp-buffer
    (yank)
    (goto-char (point-min))
    ;; Convert vector returned by json-read to a list
    (let ((entries-list (append (json-read) nil)))
      (kill-buffer)
      (hledger-fetch-entries-insert entries-list)))
  (message "Entries copied"))

(provide 'hledger-webservice)
;;; hledger-webservice.el ends here
