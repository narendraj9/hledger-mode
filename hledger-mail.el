;;; hledger-mail.el --- Extension to email reports   -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords: comm, convenience

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

(require 'cl-lib)
(require 'url)
(require 'url-http)

(defcustom hledger-reporting-buffer-name "*Personal Finance*"
  "Name of the buffer for showing or working with reports."
  :group 'hledger
  :type 'string)

(defcustom hledger-email-api-url "EMAIL_API_URL"
  "Email API end-point."
  :group 'hledger
  :type 'string)

(defcustom hledger-email-api-password "EMAIL_API_PASSWD"
  "Password for the Email API."
  :group 'hledger
  :type 'string)

(defcustom hledger-email-api-user "EMAIL_API_USER"
  "Username for Email API."
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
I am not checking the range.  You are own your own."
  :group 'hledger
  :type 'integer)

(defcustom hledger-email-reporting-retry-interval 2
  "Seconds to wait before retrying to send emails again."
  :group 'hledger
  :type 'integer)

(defcustom hledger-email-secrets-file
  (and (boundp 'secrets-file) secrets-file)
  "Path to the file containing EMAIL API credentials."
  :group 'hledger
  :type 'string)

(defvar hledger-email-next-reporting-time
  (let* ((time (current-time))
         (day (string-to-number (format-time-string "%d" time)))
         (delta-time (days-to-time (- hledger-reporting-day
                                      day))))
    (time-add time delta-time))
  "The next time beyond which we must update this variable.
It is updated after an email has been sent to the user.")


(defun hledger-make-multipart-boundary ()
  "Make the boundary for multipart/form-data.
Creates some slightly unprobably gibberish."
  (concat "x" (make-string 18 ?-) (format "%x" (random 99999999999))))

(defun hledger-make-multipart-url-data (boundary params)
  "Construct a multipart/form-data body string with BOUNDARY and PARAMS."
  (concat
   (mapconcat (lambda (kv)
                (let* ((name (format "%s" (car kv)))
                       (value (cdr kv))
                       (encoded-value (encode-coding-string value 'utf-8)))
                  (concat (concat "--" boundary) "\n"
                          "Content-Disposition: form-data; "
                          "name=\"" name "\"\n\n"
                          encoded-value "\n")))
              params
              "")
   "--" boundary "--\n"))

(defun hledger-send-email-with-mailgun (url headers)
  "Send email using Mailgun.

Returns a boolean value stating if the operation failed or succeeded.
t => success nil => failure

This function emulates the curl command as available in the Mailgun Docs:
curl -s --user USER-AND-PASSWD URL
 -F FROM='Excited User <excited@samples.mailgun.org>' \
 -F TO='devs@mailgun.net' \
 -F SUBJECT='Hello' \
 -F TEXT='Testing some Mailgun awesomeness!'

HEADERS is an assoc-list with the headers of the request.
`((authorization . AUTHORIZATION)
  (from . FROM)
  (to   . TO)
  (subject . SUBJECT)
  (text . TEXT))"
  (let* ((multipart-boundary (hledger-make-multipart-boundary))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . ,(format
                                "multipart/form-data; boundary=%s; charset=utf-8"
                                multipart-boundary))
            ("Authorization" . ,(concat
                                 "Basic "
                                 (base64-encode-string
                                  (assoc-default 'authorization headers))))))
         (url-request-data
          (hledger-make-multipart-url-data multipart-boundary
                                           (assq-delete-all 'authorization
                                                            headers))))
    ;; This is a hack until
    ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2016-08/msg00031.html
    ;; is fixed.
    (let ((_ (defadvice string-bytes (around fake-string-bytes (s))
               (setq ad-return-value (length s))))
          (_ (ad-activate 'string-bytes))
          (url-buffer (url-retrieve-synchronously url))
          (_ (ad-deactivate 'string-bytes)))
      ;; Ugly hack ends!
      (if (not url-buffer)
          nil
        (with-current-buffer url-buffer
          (url-http-parse-response)
          (= url-http-response-status 200))))))

(defun hledger-send-text-email (url user-and-password from to subject text)
  "Send an email with text body.
URL is the api-endpoint [Mailgun HTTP API endpoint].
USER-AND-PASSWORD is in the format 'user:password' and is
base64-encoded to make the Authorization header for simple
authentication.

FROM and TO are email ids for the sender and receiver respectively.
SUBJECT is the subject of the email.
TEXT is the body of the mail."
  (hledger-send-email-with-mailgun url `((authorization . ,user-and-password)
                                       (from . ,from)
                                       (to . ,to)
                                       (subject . ,subject)
                                       (text . ,text))))

(defun hledger-send-email (url user-and-password from to subject text html)
  "Send email with URL, USER-AND-PASSWORD, FROM, TO, SUBJECT and TEXT.
See `hledger-send-text-email'.  This function would send an email
with both Text and HTML parts as specified."
  (hledger-send-email-with-mailgun url `((authorization . ,user-and-password)
                                       (from . ,from)
                                       (to . ,to)
                                       (subject . ,subject)
                                       (text . ,text)
                                       (html . ,html))))


(defun hledger-compute-next-reporting-time ()
    "Computes the time we must sent the email reports."
    (let* ((now hledger-email-next-reporting-time)
           (next-month-time (time-add now (days-to-time 30)))
           (next-month-day (string-to-number
                            (format-time-string "%d" next-month-time)))
           (delta (days-to-time (- hledger-reporting-day next-month-day)))
           (next-time (time-add next-month-time delta)))
      next-time))

(defun hledger-generate-reports-to-email ()
  "Generate the text html for monthly and running reports.

Returns a cons cell with (text . html).
This requires htmlize.el"
  (require 'htmlize)
  (let ((hledger-reporting-buffer-name " *Hleder Email Reporting*"))
    (hledger-overall-report)
    (deactivate-mark t)
    (with-current-buffer hledger-reporting-buffer-name
      ;; So that no line is highlighted. The buffer is in hledger-view-mode.
      (hl-line-mode -1)
      (let* ((text (buffer-substring-no-properties (point-min)
                                                   (point-max)))
             (htmlize-output-type 'inline-css)
             (fontified-buffer  (htmlize-buffer))
             (html (with-current-buffer fontified-buffer
                     ;; Make sure that chrome uses a vertical scroll bar
                     (goto-char (point-min))
                     (search-forward "<pre")
                     (insert " style=\"white-space: pre !important; word-wrap: normal !important; overflow-x: scroll;\"")
                     (buffer-substring-no-properties (point-min)
                                                     (point-max)))))
        (kill-buffer fontified-buffer)
        `(,text . ,html)))))

(defun hledger-mail-reports ()
  "Email reports to yourself every month.
Returns t if the operation was successful."
  (interactive)
  (let* ((hledger-reporting-buffer-name "*Personal Finance Email*")
         (text-html-pair (hledger-generate-reports-to-email))
         (reports-text (car text-html-pair))
         (reports-html (cdr text-html-pair))
         (success (hledger-send-email hledger-email-api-url
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
This isn't meant to be useful for anybody other than myself.  This is extermely
inefficient."
  (require 'async)
  (async-start
   `(lambda ()
      (message "Started the new emacs process.")
      ,(async-inject-variables
        "hledger-jfile\\|load-path\\|hledger-email-secrets-file")
      (message "--> Loading hledger-mode.")
      (require 'hledger-mode)
      ;; This requires secrets. So, we don't do anything if there is
      ;; no secrets file.
      (when (file-exists-p ,hledger-email-secrets-file)
        (load ,hledger-email-secrets-file)
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
           (setq hledger-email-next-reporting-time
                 (hledger-compute-next-reporting-time))
           (customize-save-variable 'hledger-email-next-reporting-time
                                    (hledger-compute-next-reporting-time))
           (message "Hledger email reporting: Ok"))
       (message "Hledger email reporting: Failed")))))

(defun hledger-mail-monthly-report ()
  "Email monthly report if not done already for the current month."
  (when (time-less-p hledger-email-next-reporting-time (current-time))
    (if (not (ignore-errors (hledger-mail-reports)))
        (message "--> Hledger email reporting: Failed.")
      (message "--> Mail reporting was successful.")
      (setq hledger-email-next-reporting-time
                 (hledger-compute-next-reporting-time))
           (customize-save-variable 'hledger-email-next-reporting-time
                                    (hledger-compute-next-reporting-time)))))

(defun hledger-setup-mail-report-timer ()
  "Setup a timer to send monthly report when idle."
  (run-with-idle-timer 15 nil #'hledger-mail-monthly-report))

;;;###autoload
(defun hledger-enable-reporting ()
  "Report every month on `hledger-reporting-day'."
  (add-hook 'hledger-mode-hook #'hledger-setup-mail-report-timer))

(provide 'hledger-mail)
;;; hledger-mail.el ends here
