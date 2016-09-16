;;; hledger-reports.el --- Generating reports with hledger  -*- lexical-binding: t; -*-

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

;; This file contains functions that are called everywhere else to
;; generate reports using hledger binary.

;;; Code:

(require 'hledger-core)

(defconst hledger-jcompletions '("balancesheet"
                                 "daily"
                                 "incomestatement"
                                 "monthly"
                                 "overall"
                                 "print" "accounts" "balancesheet" "balance"
                                 "register")
  "Commands that can be passed to `hledger-jdo` function defined below.")

(defcustom hledger-daily-report-accounts
  "expenses"
  "Accounts for the daily report."
  :group 'hledger
  :type 'string)

(defcustom hledger-ratios-assets-accounts
  "assets"
  "Account names for total assets."
  :group 'hledger
  :type 'string)

(defcustom hledger-ratios-income-accounts
  "income"
  "Account names for total income so far."
  :group 'hledger
  :type 'string)

(defcustom hledger-ratios-liquid-asset-accounts
  "assets:bank assets:wallet"
  "Account names [separated by spaces] that contain your liquid assets"
  :group 'hledger
  :type 'string)

(defcustom hledger-ratios-essential-expense-accounts
  "expenses:housing expenses:eating expenses:family"
  "Account names [separated by spaces] that contain non-disctrionary expenses"
  :group 'hledger
  :type 'string)

(defcustom hledger-ratios-debt-accounts
  "liabilities"
  "Account names [separated by spaces] that are liabilities."
  :group 'hledger
  :type 'string)

(defcustom hledger-report-header-face
  '(:foreground "Cornsilk" :height 1.1)
  "Face for the header with date ranges in the the reports."
  :group 'hledger
  :type 'face)

(defvar hledger-last-run-command nil
  "Last run hledger-command.")

(defvar hledger-last-run-time 0
  "Last month on which a command was run.")

(defun hledger-format-time (time)
  "Format time in \"%Y-%m-%d\" "
  (format-time-string "%Y-%m-%d" time))

(defun hledger-end-date (time)
  "Format time so that it can be used as an inclusive --end date."
  (let ((next-day (time-add time
                            (days-to-time 1))))
    (hledger-format-time next-day)))

(defun hledger-friendlier-time (time)
  "Format for the user to understand: %e %B %Y"
  (format-time-string "%e %B %Y" time))

(defun hledger-nth-of-mth-month (n m)
  "Returns the nth of the mth month. Current month is the zeroth."
  (let* ((time (time-add (current-time)
                         (days-to-time (* 30 m))))
         (day (string-to-number (format-time-string "%d" time)))
         (delta-time (days-to-time (- n
                                      day))))
    (time-add time delta-time)))

(defun hledger-nth-of-this-month (n)
  "Returns the time value for the nth day of the current month"
  (hledger-nth-of-mth-month n 0))

(defun hledger-nth-of-prev-month (n)
  "Returns the nth day's time for the previous month."
  (hledger-nth-of-mth-month n -1))

(defun hledger-shell-command-to-string (command-string)
  (shell-command-to-string (concat "hledger -f "
                                   (shell-quote-argument hledger-jfile)
                                   " "
                                   command-string)))

(defun hledger-ask-and-save-buffer ()
  "Ask for saving modified buffer before any reporting commands."
  (if (and (eq major-mode 'hledger-mode)
           (buffer-modified-p)
           (yes-or-no-p (format "Save buffer %s? "
                                (buffer-name))))
      (save-buffer)
    (ignore)))

(defun hledger-go-to-starting-line ()
  "Function to go the first line that stars a new entry. Cleans up whitespace."
  (goto-char (point-max))
  (beginning-of-line)
  (while (looking-at hledger-empty-regex)
    (forward-line -1))
  (end-of-line)
  (let ((times-yet-to-move (forward-line 2)))
    (dotimes (i times-yet-to-move)
      (insert "\n"))))

(defun hledger-overlay-current-entry ()
  "Engulf an entry in an overlay."
  (interactive)
  (while (and (not (bobp))
              (not (looking-at hledger-empty-regex)))
    (forward-line -1))
  (forward-line 1)
  (setq begin (point))
  (forward-line 1)
  (while (and (not (looking-at hledger-empty-regex))
              (not (eobp)))
    (forward-line 1))
  (setq end (point))
  (setq current-entry (make-overlay begin end))
  (overlay-put current-entry 'face '(:background "black")))

(defun hledger-clear-undo-list ()
  "Empty `buffer-undo-list`."
  (buffer-disable-undo)
  (buffer-enable-undo))

(defun hledger-get-perfin-buffer (&optional keep-bufferp fetched-entriesp)
  "Get/create the `hledger-reporting-buffer-name' buffer.
If the buffer is not intended for editing, then `q` closes it.
`C-c y` copies the whole buffer to clipboard.
FIXME: Query emacs for the keys for the functions."
  (let ((jbuffer (get-buffer-create hledger-reporting-buffer-name)))
    (with-current-buffer jbuffer
      (if fetched-entriesp
          (progn
            (hledger-mode))
        (hledger-view-mode))
      (or keep-bufferp (erase-buffer)))
    jbuffer))

(defun hledger-eval-region (beg end)
  "Send selected region to hledger for evaluation."
  (interactive "r")
  (let ((command (completing-read "jdo> " hledger-jcompletions))
        (hledger-jfile (make-temp-file "hledger")))
    (write-region beg end hledger-jfile)
    (hledger-jdo command)))

(defun hledger-jentry ()
  "Make a new entry in the financial journal. Avoids editing old entries."
  (interactive)
  (find-file hledger-jfile)
  (hledger-go-to-starting-line)
  (recenter))

(defun hledger-run-command (command)
  "Runs an hledger command."
  (interactive (list (completing-read "jdo> " hledger-jcompletions)))
  (hledger-ask-and-save-buffer)
  (let ((inhibit-read-only t))
    (pcase command
      (`"incomestatement" (hledger-monthly-incomestatement))
      (`"daily" (hledger-daily-report))
      (`"monthly" (hledger-monthly-report))
      (`"overall" (hledger-overall-report)
       (pop-to-buffer hledger-reporting-buffer-name)
       (delete-other-windows))
      (_ (hledger-jdo command))))
  ;; Help other functions keep track of history.
  (setq hledger-last-run-command command)
  (when (called-interactively-p 'interactive)
    (setq hledger-last-run-time 0)))

(defun hledger-get-accounts ()
  "Returns list of account names"
  (let* ((hledger-jfile (buffer-file-name))
         (accounts-string (shell-command-to-string
                           (concat "hledger -f" hledger-jfile " accounts")))
         (accounts-list (split-string accounts-string)))
    accounts-list))

(defun hledger-jdo (command &optional keep-bufferp bury-bufferp)
  "Run a hledger command on the journal file.
Returns the buffer with the info inserted.

If KEEP-BUFFERP is non-nil, it won't erase the old contents. New
info would be prepended to the old one.

If BURY-BUFFERP is t, the `hledger-reporting-buffer-name' buffer would not be
showm to the user, this is user for using this function in elisp only
for the buffer contents. "
  (if (eq major-mode 'hledger-mode)
      (setq-local hledger-jfile (buffer-file-name)))
  (let ((jbuffer (hledger-get-perfin-buffer keep-bufferp))
        (jcommand (concat "hledger -f "
                          (shell-quote-argument hledger-jfile)
                          " "
                          command
                          " --end " (hledger-end-date (current-time)))))
    (with-current-buffer jbuffer
      (call-process-shell-command jcommand nil t nil)
      (if bury-bufferp
          (bury-buffer jbuffer)
        (pop-to-buffer jbuffer)
        (delete-other-windows))
      (goto-char (point-min))
      (setq header-line-format
            (format "Generated on: %s | %s"
                    (hledger-friendlier-time (current-time))
                    (format-time-string "%A" (current-time)))))
    jbuffer))

(defun hledger-jreg (pattern)
  "Run hledger register command."
  (interactive "spattern> ")
  (let ((jcmd (concat "register -w 150 " pattern)))
    (hledger-jdo jcmd)
    (delete-other-windows)))

(defun hledger-daily-report ()
  "Report for today's expenses.
This is subject to change based on what things I am budgeting on. 
See `hledger-daily-report-accounts'."
  (interactive)
  (with-current-buffer (hledger-get-perfin-buffer)
    (hledger-jdo (format "balance %s --begin %s --end %s"
                         (shell-quote-argument hledger-daily-report-accounts)
                         (hledger-format-time (current-time))
                         (hledger-end-date (current-time))))
    (goto-char (point-min))
    (insert (concat "Today you spent:\n"
                    "===============\n"))
    (goto-char (point-min))))

(defun hledger-monthly-incomestatement ()
  "Incomestatement report but monthly.  You can have move back
and forth in time in the personal finance buffer. I feel that the
complete incomestatement isn't much useful for me. "
  (interactive)
  (let* ((beg-time (hledger-nth-of-prev-month hledger-reporting-day))
         (end-time (hledger-nth-of-this-month hledger-reporting-day))
         (beg-time-string (hledger-format-time beg-time))
         (end-time-string (hledger-format-time end-time)))
    (with-current-buffer (hledger-get-perfin-buffer)
      (hledger-jdo (format "incomestatement --flat -b %s -e %s --depth 2"
                           beg-time-string
                           end-time-string))
      (goto-char (point-min))
      ;; Sort revenues
      (when (search-forward "Revenues:")
        (forward-line)
        (unless (looking-at "--")
          (let ((beg (point)))
            (while (not (looking-at "--"))
              (forward-line))
            (sort-numeric-fields 2 beg (point))
            (reverse-region beg (point)))))
      ;; Same thing again. Need to abstract this sorting stuff. 
      (when (search-forward "Expenses:")
        (forward-line)
        (unless (looking-at "--")
          (let ((beg (point)))
            (while (not (looking-at "--"))
              (forward-line))
            (sort-numeric-fields 2 beg (point))
            (reverse-region beg (point)))))
      (goto-char (point-min))
      (insert (hledger-generate-report-header beg-time end-time)))))

(defun hledger-monthly-report (&optional keep-bufferp bury-bufferp)
  "Build the monthly report.
I make reports from 15th of the Month to 15th of the next month.
To configure this, see `hledger-reporting-day'."
  (interactive)
  (let* ((beg-time (hledger-nth-of-prev-month hledger-reporting-day))
         (end-time (hledger-nth-of-this-month hledger-reporting-day))
         (beg-time-string (hledger-format-time beg-time))
         (end-time-string (hledger-format-time end-time)))
    (hledger-jdo (format "balance expenses income -b %s -e %s --depth 2"
                         beg-time-string
                         end-time-string)
                 keep-bufferp
                 bury-bufferp)
    (with-current-buffer (hledger-get-perfin-buffer t)
      (goto-char (point-min))
      (insert (hledger-generate-report-header beg-time end-time))
      (insert (concat "Income Statement\n"
                      "================\n"))
      (let ((beg (point)))
        (while (not (looking-at "--"))
          (forward-line))
        (sort-numeric-fields 2 beg (point))
        (reverse-region beg (point)))
      (forward-line 2)
      (insert "\n\n"))
      ;; Back to the start
      (goto-char (point-min))
      (when bury-bufferp
        (bury-buffer))))

(defun hledger-running-report (&optional keep-bufferp bury-bufferp)
  "Show the balance report for the past 5 months."
  (interactive)
  (let* ((beg-time (time-subtract (current-time) (days-to-time (* 4 31))))
         (beg-time-string (hledger-format-time beg-time))
         (end-time-string (hledger-format-time (current-time))))
    (hledger-jdo (format "balance expenses income --depth 2 -MTA -b %s -e %s"
                         beg-time-string
                         end-time-string)
                 keep-bufferp
                 bury-bufferp)
    (when (not bury-bufferp)
      ;; This is because the running report is usually very wide.
      (pop-to-buffer (hledger-get-perfin-buffer t))
      (delete-other-windows))
    ;; Let's sort according to the average column now
    (with-current-buffer (hledger-get-perfin-buffer t)
      (goto-char (point-min))
      (while (not (looking-at "=="))
        (forward-line))
      (forward-line)
      (let ((beg (point)))
        (while (not (looking-at "--"))
          (forward-line))
        (sort-numeric-fields -1 beg (point))
        (reverse-region beg (point)))
      (goto-char (point-max))
      (insert "\nExpanded Running Report\n=======================\n\n"))
    (hledger-jdo (format "balance expenses income --tree -MTA -b %s -e %s"
                         beg-time-string
                         end-time-string)
                 t
                 bury-bufferp)))

(defun hledger-compute-total (accounts-string &optional beg  end)
  "Computes the total for given accounts.
This function depends upon how `hledger-bin' prints data to the console.
If that changes, things will break. BEG and END are dates."
  (let* ((date-now (hledger-end-date (current-time)))
         (output (hledger-shell-command-to-string
                  (concat " balance "
                          accounts-string
                          (if beg (concat " --begin " beg) "") 
                          " --end " (or end date-now)
                          " --depth 1"))))
    (string-to-number (nth 1 (split-string output)))))

(defun hledger-compute-totals (accounts-list &optional beg end)
  "Computes the total for a list of accounts. See `hledger-compute-total'."
  (let* ((date-now (hledger-end-date (current-time)))
         (output (hledger-shell-command-to-string
                  (concat " balance "
                          (mapconcat 'identity accounts-list " ")
                          (if beg (concat " --begin " beg) "") 
                          " --end " (or end date-now)
                          " --depth 1"
                          " --format '\"%(account)\" %(total)' ")))
         (elisp-string (concat "("
                               (replace-regexp-in-string
                                (concat hledger-currency-string
                                        "\\|-")
                                ""
                                output)
                               ")"))
         (result (car (read-from-string elisp-string))))
    result))

(defun hledger-generate-ratios ()
  "Computes various personal finance ratios:

Computes the emergency fund ratio for the current month.
EFR = (Current liquid assets)/(Monthly essential expenses)

I consider expenses on housing, eating and family to be
non-discretionary. Shoot for keeping it 6. Too high isn't
efficient. Too low isn't safe.

Computes the current ratio which gives you an estimate of how your current
asset vs liability situation is. Current ratio = assets / liabilities

Debt ratio = liabilities / assets

Returns a plist of the ratios.

Note: Currently this is extermely inefficient. It spawns hledger
three times.

"
  (interactive)
  (let* ((date-now (hledger-end-date (current-time)))
         (reporting-date-an-year-ago (hledger-format-time (hledger-nth-of-mth-month
                                                           hledger-reporting-day
                                                           -12)))
         (reporting-date-now (hledger-end-date (hledger-nth-of-this-month
                                                hledger-reporting-day)))

         (totals-plist-1 (hledger-compute-totals
                          (list hledger-ratios-assets-accounts
                                hledger-ratios-income-accounts
                                hledger-ratios-essential-expense-accounts)
                          reporting-date-an-year-ago
                          reporting-date-now))
         ;; For average balances
         (total-assets
          (lax-plist-get totals-plist-1
                         (hledger-get-top-level-acount hledger-ratios-assets-accounts)))
         (total-income
          (lax-plist-get totals-plist-1
                         (hledger-get-top-level-acount hledger-ratios-income-accounts)))
         (total-expenses
          (lax-plist-get totals-plist-1
                         (hledger-get-top-level-acount hledger-ratios-essential-expense-accounts)))
         ;; For current balances
         (totals-plist-2 (hledger-compute-totals
                          (list hledger-ratios-liquid-asset-accounts
                                hledger-ratios-debt-accounts)))
         (liquid-assets
          (lax-plist-get totals-plist-2
                         (hledger-get-top-level-acount hledger-ratios-liquid-asset-accounts)))
         (liabilities
          (lax-plist-get totals-plist-2
                         (hledger-get-top-level-acount hledger-ratios-debt-accounts)))
         (monthly-expenses (/ total-expenses 12))
         (monthly-income (/ total-income 12.0))
         (monthly-savings (/ total-assets 12.0)))
    (list 'avg-income (* monthly-income 1.0)              ;; Monthly income
          'avg-expenses (* monthly-expenses 1.0)          ;; Average expenses
          'efr (/ liquid-assets (* monthly-expenses 1.0)) ;; Emergency-fund-ratio
          'cr  (/ liquid-assets (* liabilities 1.0))      ;; Current ratio
          'sr  (/ monthly-savings monthly-income)         ;; Savings ratio
          'dr (/ liabilities (* liquid-assets 1.0)))))    ;; Debt ratio


(defun hledger-overall-report ()
  "A combination of all the relevant reports."
  (interactive)
  (message "Generating overall report...")
  (let ((inhibit-read-only t))
    (hledger-running-report nil t)
    (hledger-monthly-report t t)
    (with-current-buffer (hledger-get-perfin-buffer t)
      (let* ((ratios (hledger-generate-ratios))
             (efr (plist-get ratios 'efr))
             (cr (plist-get ratios 'cr))
             (dr (plist-get ratios 'dr))
             (sr (plist-get ratios 'sr))
             (avg-income (plist-get ratios 'avg-income))
             (avg-expenses (plist-get ratios 'avg-expenses)))
        (goto-char (point-min))
        (forward-line 2)
        (insert (format "
╔══════════════════════════════════════╦══════════════════════════════════════════╗ 

   Emergency Fund Ratio: %-18.2fSavings Ratio: %.2f
   Current Ratio: %-25.2fAverage Income: ₹ %.0f/month                        
   Debt Ratio: %-28.2fAverage Expenses: ₹ %.0f/month           

╚══════════════════════════════════════╩══════════════════════════════════════════╝
                                                               
"                                                             
                        efr sr
                        cr  avg-income
                        dr  avg-expenses)))                             
      (goto-char (point-min))))
  (message "Done!"))

(defun hledger-run-command-for-month (m command)
  "Runs an hledger COMMAND for Mth month.
This is the reason dynamic scoping is cool sometimes."
  (letf (((symbol-function 'current-time)
          (let ((time (hledger-nth-of-mth-month
                       (string-to-number (format-time-string "%d" (current-time)))
                       m)))
            `(lambda () ',time))))
    (hledger-run-command command)))

(defun hledger-run-command-for-day (m command)
  "Runs an hledger COMMAND for Mth day relative to today."
  (letf (((symbol-function 'current-time)
          (let ((time (time-add
                       (current-time)
                       (days-to-time m))))
            `(lambda () ',time))))
    (hledger-run-command command)))

(defun hledger-generate-report-header (beg-time end-time)
  "Generates report header with dates."
  (let* ((header-dates (format "%s - %s"
                               (format-time-string "%e %b %Y" beg-time)
                               (format-time-string "%e %b %Y" end-time)))
         (header-title "Report : ")
         (header-filler (make-string (+ (length header-dates)
                                        (length header-title))
                                     ?=)))
    (propertize (format "%s %s\n%s=\n\n"
                        header-title
                        header-dates
                        header-filler)
                'font-lock-face hledger-report-header-face)))

(defun hledger-prev-report ()
  "Takes your current report back in time.
To be called once you have run a report that sets `hledger-last-run-command'."
  (interactive)
  (setq hledger-last-run-time (1- hledger-last-run-time))
  (pcase hledger-last-run-command
    (`"daily" (hledger-run-command-for-day hledger-last-run-time
                                           hledger-last-run-command))
    (_ (hledger-run-command-for-month hledger-last-run-time
                                    hledger-last-run-command))))

(defun hledger-next-report ()
  "Takes your report forward in time.
See `hledger-prev-report'."
  (interactive)
  (setq hledger-last-run-time (1+ hledger-last-run-time))
  (pcase hledger-last-run-command
    (`"daily" (hledger-run-command-for-day hledger-last-run-time
                                           hledger-last-run-command))
    (_ (hledger-run-command-for-month hledger-last-run-time
                                      hledger-last-run-command))))

(defun hledger-present-report ()
  "Resets time for the current report.
See `hledger-prev-report'."
  (interactive)
  (setq hledger-last-run-time 0)
  (pcase hledger-last-run-command
    (`"daily" (hledger-run-command-for-day hledger-last-run-time
                                           hledger-last-run-command))
    (_ (hledger-run-command-for-month hledger-last-run-time
                                      hledger-last-run-command))))

(defun hledger-make-reporting-buffer-read-only ()
  "Make the `hledger-reporting-buffer-name' read-only."
  (with-current-buffer (hledger-get-perfin-buffer t)
    (set-text-properties (point-min)
                         (point-max)
                         '(read-only t front-sticky t))))

(defun hledger-get-top-level-acount (acc-string)
  "Returns the top-level account from ACC-STRING."
  (car (split-string acc-string ":")))

(provide 'hledger-reports)
;;; hledger-reports.el ends here
