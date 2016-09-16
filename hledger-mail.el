;;; hledger-mail.el --- Extension to email reports   -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords: comm, convenience
;; Package-Requires: ((utils "0.1"))

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

;; This extension manages sending monthly emails containing hleger
;; reports to the user.

;;; Code:

(require 'hledger-core)
(require 'hledger-reports)
(require 'utils) 

(defcustom hledger-reporting-buffer-name "*Personal Finance*"
  "Name of the buffer for showing or working with reports."
  :group 'hledger
  :type 'string)

(defcustom hledger-email-api-url "EMAIL_API_URL"
  "Email API end-point."
  :group 'hledger
  :type 'string)

(defcustom hledger-email-api-password "EMAIL_API_PASSWD"
  "Password for the Email API"
  :group 'hledger
  :type 'string)

(defcustom hledger-email-api-user "EMAIL_API_USER"
  "Username for Email API"
  :group 'hledger
  :type 'string)

(defcustom hledger-email-api-sender "SENDER_EMAIL_ID"
  "Email id for the sender of your reports."
  :group 'hledger
  :type 'string)

(defcustom hledger-email-api-recipient "RECIPIENT_EMAIL_ID"
  "Email id for the receiver of your reports, i.e. you!"
  :group 'hledger
  :type 'string)

(defcustom hledger-reporting-day 15
  "Day of the month for sending email reports.
I am not checking the range. You are own your own. "
  :group 'hledger
  :type 'integer)

(defcustom hledger-email-reporting-retry-interval 2
  "Seconds to wait before retrying to send emails again."
  :group 'hledger
  :type 'integer)

(defcustom hledger-email-next-reporting-time
  (let* ((time (current-time))
         (day (string-to-number (format-time-string "%d" time)))
         (delta-time (days-to-time (- hledger-reporting-day
                                      day))))
    (time-add time delta-time))
  "The next time beyond which we must update this variable.
It is updated after an email has been sent to the user.")

(defun hledger-compute-next-reporting-time ()
    "Computes the time we must sent the email reports. "
    (let* ((now hledger-email-next-reporting-time)
           (next-month-time (time-add now (days-to-time 30)))
           (next-month-day (string-to-number
                            (format-time-string "%d" next-month-time)))
           (delta (days-to-time (- 15 next-month-day)))
           (next-time (time-add next-month-time delta)))
      next-time))

(defun hledger-generate-reports-to-email ()
  "Generate the text html for monthly and running reports.

Returns a cons cell with (text . html).
This requires htmlize.el"
  (require 'htmlize)
  (hledger-overall-report)
  (deactivate-mark t)
  (with-current-buffer hledger-reporting-buffer-name
    ;; So that no line is highlighted. The buffer is in hledger-view-mode.
    (hl-line-mode -1)
    (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
           (htmlize-output-type 'inline-css)
           (fontified-buffer  (htmlize-buffer))
           (html (with-current-buffer fontified-buffer
                   ;; Make sure that chrome uses a vertical scroll bar
                   (goto-char (point-min))
                   (search-forward "<pre")
                   (insert " style=\"white-space: pre !important; word-wrap: normal !important; overflow-x: scroll;\"")
                   (buffer-substring-no-properties (point-min) (point-max)))))
      (kill-buffer fontified-buffer)
      `(,text . ,html))))

(defun hledger-mail-reports ()
  "Email reports to yourself every month.
This requires utils.el which is available in utils/ alonside
hledger-mode/ directory.

Returns t if the operation was successful.
"
  (interactive)
  (let* ((hledger-reporting-buffer-name "*Personal Finance Email*")
         (text-html-pair (hledger-generate-reports-to-email))
         (reports-text (car text-html-pair))
         (reports-html (cdr text-html-pair))
         (success (utils-send-email hledger-email-api-url
                                    (concat hledger-email-api-user ":"
                                            hledger-email-api-password)
                                    hledger-email-api-sender
                                    hledger-email-api-recipient
                                    (format "Monthly Financial Report [%s]"
                                            (format-time-string "%B %Y"))
                                    reports-text
                                    reports-html)))
    (kill-buffer hledger-reporting-buffer-name)
    (message (if success
                 "Hledger email reporting: Ok"
               "Hledger email reporting: Failed"))
    success))

(defun hledger-mail-reports-run-async-task ()
    "Async task for emailing the reports.
This isn't meant to be useful for anybody other than myself. This is extermely
inefficient."
    (require 'async)
    (async-start
     `(lambda ()
        (message "Started the new emacs process.")
        (setq load-path (quote ,load-path))
        (setq hledger-jfile ,hledger-jfile)
        (message "--> Loading hledger-mode.")
        (require 'hledger-mode)
        ;; This requires secrets. So, we don't do anything if there is
        ;; no secrets file.
        (when (file-exists-p ,secrets-file)
          (load ,secrets-file)
          (let ((epoch (current-time)))
            ;; Seed waiting time. To make exponential back-off simpler.
            ;; Sleeping times go like this: t(n) = 2 * Σ t(i) for all i < n
            ;; and t(0) = `hledger-email-reporting-retry-interval'.
            (message "--> Sleeping for %.0f seconds"
                     hledger-email-reporting-retry-interval)
            (sleep-for hledger-email-reporting-retry-interval)
            (while (not (ignore-errors (hledger-mail-reports)))
              (message "--> Hledger email reporting: Failed.")
              (let ((waiting-time (* 2 (time-to-seconds
                                        (time-subtract (current-time)
                                                       epoch)))))
                (message "--> Sleeping for %.0f seconds" waiting-time)
                (sleep-for waiting-time)))
            t)))
     (lambda (success)
       (if success
           (progn
             (customize-save-variable 'hledger-email-next-reporting-time
                                      (hledger-compute-next-reporting-time))
             (message "Hledger email reporting: Ok"))
         (message "Hledger email reporting: Failed")))))

;;;###autoload
(defun hledger-enable-reporting ()
  "Report every month on `hledger-reporting-day'."
  (when (time-less-p hledger-email-next-reporting-time (current-time))
    (hledger-mail-reports-run-async-task)))

(provide 'hledger-mail)
;;; hledger-mail.el ends here
