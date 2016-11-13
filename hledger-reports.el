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
(require 'cl-lib)

(defconst hledger-jcompletions '("balancesheet"
                                 "daily"
                                 "incomestatement"
                                 "overall"
                                 "print"
                                 "accounts"
                                 "balance"
                                 "register")
  "Commands that can be passed to `hledger-jdo` function defined below.")

(defcustom hledger-top-asset-account "assets"
  "Top level assets acccount."
  :group 'hledger
  :type 'string)

(defcustom hledger-top-expense-account "expenses"
  "Top level expense account."
  :group 'hledger
  :type 'string)

(defcustom hledger-top-income-account "income"
  "Top level expense account."
  :group 'hledger
  :type 'string)

(defcustom hledger-show-only-unstarred-p t
  "Show only the un-tainted entries.
I taint entries with a star, to declare that they haven't been effective yet."
  :group 'hledger
  :type 'boolean)

(defcustom hledger-running-report-months
  5
  "Number of months to show in the running report."
  :group 'hledger
  :type 'number)

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
  "Account names [separated by spaces] that contain your liquid assets."
  :group 'hledger
  :type 'string)

(defcustom hledger-ratios-essential-expense-accounts
  "expenses:housing expenses:eating expenses:family"
  "Account names [separated by spaces] that contain non-disctrionary expenses."
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

(defcustom hledger-account-balance-expand-face
  '(:foreground "Cornsilk" :background "DarkSlateGray")
  "Face for the expanded account names with their balances in current period."
  :group 'hledger
  :type 'face)

(defvar hledger-last-run-command nil
  "Last run hledger-command.")

(defvar hledger-last-run-time 0
  "Last month on which a command was run.")

(defvar hledger-ratios-summary nil
  "Summary for the ratios in overall report.")

(defun hledger-format-time (time)
  "Format TIME in \"%Y-%m-%d\"."
  (format-time-string "%Y-%m-%d" time))

(defun hledger-end-date (time)
  "Format TIME so that it can be used as an inclusive --end date."
  (let ((next-day (time-add time
                            (days-to-time 1))))
    (hledger-format-time next-day)))

(defun hledger-friendlier-time (time)
  "Format TIME for the user to understand: %e %B %Y."
  (format-time-string "%e %B %Y" time))

(defun hledger-nth-of-mth-month (n m)
  "Return the Nth of the Mth month.  Current month is the zeroth.

Note: uses `calendar-increment-month' to go back and forth in
time."
  (let* ((time-now (current-time))
         (year-now (string-to-number (format-time-string "%Y" time-now)))
         (month-now (string-to-number (format-time-string "%m" time-now))))
    (calendar-increment-month month-now year-now m)
    ;; Now, we want the Nth of month-now and year-now.
    (encode-time 0 0 0 n month-now year-now)))

(defun hledger-nth-of-this-month (n)
  "Return the time value for the Nth day of the current month."
  (hledger-nth-of-mth-month n 0))

(defun hledger-nth-of-prev-month (n)
  "Return the Nth day's time for the previous month."
  (hledger-nth-of-mth-month n -1))

(defun hledger-shell-command-to-string (command-string)
  "Return the result of running COMMAND-STRING has an hledger command."
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
  "Function to go the first line that stars a new entry.  Cleans up whitespace."
  (goto-char (point-max))
  (beginning-of-line)
  (while (looking-at hledger-empty-regex)
    (forward-line -1))
  (end-of-line)
  (let ((times-yet-to-move (forward-line 2)))
    (insert (make-string times-yet-to-move ?\n))))

(defun hledger-get-perfin-buffer (&optional keep-bufferp fetched-entriesp)
  "Get/create the `hledger-reporting-buffer-name' buffer.
If the buffer is not intended for editing, then `q` closes it.
`C-c y` copies the whole buffer to clipboard.  FIXME: Query Emacs
for the keys for the functions.

Optional argument KEEP-BUFFERP
if non-nil the `hledger-reporting-buffer-name' is re-used without
erasing its contents.

Optional argument FETCHED-ENTRIESP if
non-nil, it lands us in the `hledger-mode' ."
  (let ((jbuffer (get-buffer-create hledger-reporting-buffer-name)))
    (with-current-buffer jbuffer
      (if fetched-entriesp
          (progn
            (hledger-mode))
        (hledger-view-mode))
      (or keep-bufferp (progn (delete-region (point-min) (point-max))
                              (delete-all-overlays))))
    jbuffer))


(defun hledger-jentry ()
  "Make a new entry in the financial journal.  Avoids editing old entries."
  (interactive)
  (find-file hledger-jfile)
  (hledger-go-to-starting-line)
  (recenter))

(defun hledger-run-command (command)
  "Run an hledger COMMAND."
  (interactive (list (completing-read "jdo> " hledger-jcompletions)))
  (hledger-ask-and-save-buffer)
  (let ((inhibit-read-only t))
    (pcase command
      (`"incomestatement" (hledger-monthly-incomestatement))
      (`"daily" (hledger-daily-report))
      (`"overall" (hledger-overall-report)
       (pop-to-buffer hledger-reporting-buffer-name)
       (delete-other-windows))
      (_ (hledger-jdo command))))
  ;; Help other functions keep track of history.
  (setq hledger-last-run-command command)
  (when (called-interactively-p 'interactive)
    (setq hledger-last-run-time 0)))

(defun hledger-get-accounts ()
  "Return list of account names."
  (let* ((hledger-jfile (buffer-file-name))
         (accounts-string (shell-command-to-string
                           (concat "hledger -f" hledger-jfile " accounts")))
         (accounts-list (split-string accounts-string)))
    accounts-list))

(defun hledger-jdo (command &optional keep-bufferp bury-bufferp)
  "Run a hledger COMMAND on the journal file.
Returns the buffer with the info inserted.

If KEEP-BUFFERP is non-nil, it won't erase the old contents.  New
info would be prepended to the old one.

If BURY-BUFFERP is t, the `hledger-reporting-buffer-name' buffer
would not be showm to the user, this is user for using this
function in elisp only for the buffer contents.

The position of point remains unaltered after this function
call.  This is for letting the caller transform the output more
easily."
  (let ((jbuffer (hledger-get-perfin-buffer keep-bufferp))
        (jcommand (concat "hledger -f "
                          (shell-quote-argument hledger-jfile)
                          " "
                          command)))
    (with-current-buffer jbuffer
      (let ((here (point)))
        (call-process-shell-command jcommand nil t nil)
        ;; Keep the pointer where it was before executing the hledger command
        (goto-char here))
      (if bury-bufferp
          (bury-buffer jbuffer)
        (pop-to-buffer jbuffer)
        (delete-other-windows))
      (setq header-line-format
            (format "Generated on: %s | %s"
                    (hledger-friendlier-time (current-time))
                    (format-time-string "%A" (current-time)))))
    jbuffer))

(defun hledger-jreg (pattern)
  "Run hledger register command with PATTERN as argument."
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
    (let ((reporting-since (hledger-compute-last-reporting-time))
          (beg-time-string (hledger-format-time (current-time)))
          (end-time-string (hledger-end-date (current-time))))
      (hledger-jdo (format "balance %s --begin %s --end %s"
                           hledger-daily-report-accounts
                           beg-time-string
                           end-time-string))
      (goto-char (point-min))
      (insert (concat "Today you spent:\n"
                      (make-string 20 ?=)
                      "\n"))
      (goto-char (point-max))
      (insert (concat "\n\nSince "
                      (hledger-friendlier-time reporting-since)
                      "\n"
                      (make-string 20 ?=)
                      "\n"))
      (let ((beg-time-string (hledger-format-time ()))))
      (hledger-jdo (format "balance %s --begin %s --end %s"
                           hledger-daily-report-accounts
                           (hledger-format-time reporting-since)
                           (hledger-end-date (current-time)))
                   t)
      (goto-char (point-min)))))

(defun hledger-monthly-incomestatement (&optional hide-header-p)
  "Incomestatement report but monthly.
You can have move back
and forth in time in the personal finance buffer.  I feel that the
complete incomestatement isn't much useful for me.
Optional argument HIDE-HEADER-P if non-nil, header line showing duration isn't shown."
  (interactive)
  (let* ((beg-time (hledger-nth-of-prev-month hledger-reporting-day))
         (end-time (hledger-nth-of-this-month hledger-reporting-day))
         (beg-time-string (hledger-format-time beg-time))
         (end-time-string (hledger-format-time end-time)))
    (with-current-buffer (hledger-get-perfin-buffer)
      (when (not hide-header-p)
        (insert (hledger-generate-report-header beg-time end-time))
        (forward-line 2))
      (hledger-jdo (format "incomestatement --flat -b %s -e %s --depth 2"
                           beg-time-string
                           end-time-string)
                   t)

      ;; Sort revenues | Ignore errors encountered during this.
      (ignore-errors (when (search-forward "Revenues:")
                       (forward-line)
                       (unless (looking-at "--")
                         (let ((beg (point)))
                           (while (not (looking-at "--"))
                             (forward-line))
                           (sort-numeric-fields 2 beg (point))
                           (reverse-region beg (point))))))
      ;; Same thing again. Need to abstract this sorting stuff.
      (ignore-errors (when (search-forward "Expenses:")
                       (forward-line)
                       (unless (looking-at "--")
                         (let ((beg (point)))
                           (while (not (looking-at "--"))
                             (forward-line))
                           (sort-numeric-fields 2 beg (point))
                           (reverse-region beg (point))))))
      (goto-char (point-max))
      (insert "\n\n"))))

(defun hledger-running-report (&optional keep-bufferp bury-bufferp)
  "Show the balance report for the past 5 months.

Optional argument KEEP-BUFFERP if non-nil, the reporting buffer's
old contents are kept intact.

Optional argument BURY-BUFFERP if non-nil, the reporting buffer
isn't switched to."
  (interactive)
  (let* ((beg-time-string (hledger-format-time (hledger-nth-of-mth-month
                                                hledger-reporting-day
                                                (- hledger-running-report-months))))
         (end-time-string (hledger-format-time (hledger-nth-of-mth-month
                                                hledger-reporting-day
                                                0))))
    (hledger-jdo (format "balance %s %s --depth 2 -A -p %s"
                         hledger-top-expense-account
                         hledger-top-income-account
                         (shell-quote-argument
                          (format "every %sth day of month from %s to %s"
                                  hledger-reporting-day
                                  beg-time-string
                                  end-time-string)))
                 keep-bufferp
                 bury-bufferp)
    (when (not bury-bufferp)
      ;; This is because the running report is usually very wide.
      (pop-to-buffer (hledger-get-perfin-buffer t))
      (delete-other-windows))
    (with-current-buffer (hledger-get-perfin-buffer t)
      ;; Let's sort according to the average column now
      (ignore-errors (while (not (looking-at "=="))
                       (forward-line))
                     (forward-line)
                     (let ((beg (point)))
                       (while (not (looking-at "--"))
                         (forward-line))
                       (sort-numeric-fields -1 beg (point))
                       (reverse-region beg (point))))
      ;; Now adding the expanded Report with all accounts expanded.
      (goto-char (point-max))
      (insert "\nExpanded Running Report\n=======================\n\n")
      (hledger-jdo (format "balance %s %s --tree -A -p %s"
                           hledger-top-expense-account
                           hledger-top-asset-account
                           (shell-quote-argument (format "every %sth day of month from %s to %s"
                                                         hledger-reporting-day
                                                         beg-time-string
                                                         end-time-string)))
                   t
                   bury-bufferp))))


(defun hledger-compute-last-reporting-time ()
    "Return the time since when we are preparing the report."
    (let ((day (string-to-number (format-time-string "%d"))))
      (if (> day hledger-reporting-day)
          (hledger-nth-of-this-month hledger-reporting-day)
        (hledger-nth-of-prev-month hledger-reporting-day))))


(defun hledger-compute-total (accounts-string &optional beg  end)
  "Computes the total for given accounts in ACCOUNTS-STRING.
This function depends upon how `hledger-bin' prints data to the console.
If that changes, things will break.  BEG and END are dates."
  (let* ((date-now (hledger-end-date (current-time)))
         (output (hledger-shell-command-to-string
                  (concat " balance "
                          accounts-string
                          (if beg (concat " --begin " beg) "")
                          " --end " (or end date-now)
                          " --depth 1"))))
    (string-to-number (nth 1 (split-string output)))))

(defun hledger-compute-totals (accounts-list &optional beg end)
  "Computes the total for a list of accounts in ACCOUNTS-LIST.
sSee `hledger-compute-total'.
Optional argument BEG beginning date string for journal entries to consider.
Optional argument END end date string for journal entries to consider."
  (let* ((date-now (hledger-end-date (current-time)))
         (output (hledger-shell-command-to-string
                  (concat " balance "
                          (mapconcat 'identity accounts-list " ")
                          (if beg (concat " --begin " beg) "")
                          " --end " (or end date-now)
                          " --depth 1"
                          " --format "
                          (shell-quote-argument "\"%(account)\" %(total) "))))
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
non-discretionary.  Shoot for keeping it 6. Too high isn't
efficient.  Too low isn't safe.

Computes the current ratio which gives you an estimate of how your current
asset vs liability situation is.  Current ratio = assets / liabilities

Debt ratio = liabilities / assets

Returns a plist of the ratios.

Note: Currently this is extermely inefficient.  It spawns hledger
three times."
  (interactive)
  (let* ((reporting-date-an-year-ago (hledger-format-time (hledger-nth-of-mth-month
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
         (total-assets-accumulated-this-year
          (or (lax-plist-get totals-plist-1
                             (hledger-get-top-level-acount hledger-ratios-assets-accounts))
              0))
         (total-income-accumulated-this-year
          (or (lax-plist-get totals-plist-1
                             (hledger-get-top-level-acount hledger-ratios-income-accounts))
              0))
         (total-essential-expenses-this-year
          (or (lax-plist-get totals-plist-1
                             (hledger-get-top-level-acount hledger-ratios-essential-expense-accounts))
              0))

         ;; For current balances
         (totals-plist-2 (hledger-compute-totals
                          (list hledger-ratios-liquid-asset-accounts
                                hledger-ratios-debt-accounts)))
         (liquid-assets
          (or (lax-plist-get totals-plist-2
                             (hledger-get-top-level-acount hledger-ratios-liquid-asset-accounts))
              0))
         (liabilities
          (or (lax-plist-get totals-plist-2
                             (hledger-get-top-level-acount hledger-ratios-debt-accounts))
              0))

         ;; For ther rest
         (total-assets (hledger-compute-total hledger-top-asset-account))
         (total-expenses (hledger-compute-total hledger-top-expense-account
                                                reporting-date-an-year-ago
                                                reporting-date-now))

         (monthly-total-expenses (/ total-expenses 12.0))
         (monthly-essential-expenses (/ total-essential-expenses-this-year 12.0))
         (monthly-income (/ total-income-accumulated-this-year 12.0))
         (monthly-savings (/ total-assets-accumulated-this-year 12.0)))
    (list 'avg-income (* monthly-income 1.0)                        ;; Monthly income
          'avg-expenses (* monthly-total-expenses 1.0)              ;; Average expenses
          'efr (/ liquid-assets (* monthly-essential-expenses 1.0)) ;; Emergency-fund-ratio
          'cr  (/ liquid-assets (* liabilities 1.0))                ;; Current ratio
          'sr  (/ monthly-savings monthly-income)                   ;; Savings ratio
          'dr (/ liabilities (* total-assets 1.0)))))               ;; Debt ratio


(defun hledger-summarize-ratios (ratios)
  "Return a string summary of RATIOS."
  (let ((efr (plist-get ratios 'efr))
        (cr (plist-get ratios 'cr))
        (dr (plist-get ratios 'dr))
        (sr (plist-get ratios 'sr)))
    (format
     (concat
      (make-string 80 ?=) "\n"
      "• Your current assets would be consumed in %.2f months with this lifestyle.\n"
      "• Your liquid assets are %.2f times your liabilities/debt.\n"
      "• %.2f%% of your total assets are borrowed.\n"
      "• For the past one year, you have been saving %.2f%% of your average income.\n"
      (make-string 80 ?=) "\n")
     efr
     cr
     (* dr 100.0)
     (* sr 100.0))))

(defun hledger-overall-report ()
  "A combination of all the relevant reports."
  (interactive)
  (message "Generating overall report...")
  (let ((inhibit-read-only t))
    (hledger-monthly-incomestatement)
    (hledger-running-report t t)
    (with-current-buffer (hledger-get-perfin-buffer t)
      (let* ((ratios (hledger-generate-ratios))
             (efr (plist-get ratios 'efr))
             (cr (plist-get ratios 'cr))
             (dr (plist-get ratios 'dr))
             (sr (plist-get ratios 'sr))
             (avg-income (plist-get ratios 'avg-income))
             (avg-expenses (plist-get ratios 'avg-expenses))
             (summary (hledger-summarize-ratios ratios)))
        (goto-char (point-min))
        (forward-line 2)
        (insert (format "
╔══════════════════════════════════════╦══════════════════════════════════════════╗

   Emergency Fund Ratio: %-18.2fSavings Ratio: %.2f
   Current Ratio: %-25.2fAverage Income: %s %.0f/month
   Debt Ratio: %-28.2fAverage Expenses: %s %.0f/month

╚══════════════════════════════════════╩══════════════════════════════════════════╝

"
                        efr sr
                        cr  hledger-currency-string avg-income
                        dr  hledger-currency-string avg-expenses))
        ;; Let's update the ratios summary
        (setq hledger-ratios-summary summary)
        (setq hledger-ratios-summary-point (point)))
      (goto-char (point-min))
      (message "Done!"))))

(defun hledger-run-fn-for-month (m command)
  "Run for Mth month, hledger command string COMMAND.
This is the reason dynamic scoping is cool sometimes."
  (cl-letf (((symbol-function 'current-time)
             (let ((time (hledger-nth-of-mth-month
                          hledger-reporting-day
                          m)))
               `(lambda () ',time))))
    (funcall command)))

(defun hledger-run-fn-for-day (m command)
  "Run for Mth day relative to today, hledger command string COMMAND."
  (cl-letf (((symbol-function 'current-time)
             (let ((time (time-add
                          (current-time)
                          (days-to-time m))))
               `(lambda () ',time))))
    (funcall command)))

(defun hledger-run-command-for-month (m command)
  "Run *hledger* command for month M where COMMAND is a string."
  (hledger-run-fn-for-month m (lambda ()
                                (hledger-run-command command))))

(defun hledger-run-command-for-day (m command)
  "Run *hledger* command for day M where COMMAND is a string."
  (hledger-run-fn-for-day m (lambda ()
                              (hledger-run-command command))))

(defun hledger-generate-report-header (beg-time end-time)
  "Generate report header with dates between times BEG-TIME and END-TIME."
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

(defun hledger-expand-account-for-month ()
  "Expands account for the month according to `hledger-last-run-time'."
  (interactive)
  (if (equal hledger-last-run-command "daily")
      (message "No expansion for daily report.")
    (hledger-run-fn-for-month hledger-last-run-time
                              'hledger-expand-account-for-this-month)))

(defun hledger-expand-account-for-this-month ()
  "Expand the balance for account in the current line."
  (save-excursion
    (forward-line 0)
    (when (search-forward-regexp hledger-account-regex (line-end-position) t)
      (let* ((account (substring-no-properties (match-string 0)))
             (drop-count (length (split-string account ":")))
             (beg-time (hledger-nth-of-prev-month hledger-reporting-day))
             (end-time (hledger-nth-of-this-month hledger-reporting-day))
             (beg-time-string (hledger-format-time beg-time))
             (end-time-string (hledger-format-time end-time))
             (balance-report (hledger-shell-command-to-string
                              (format "balance %s --flat -b %s -e %s --drop %d -N"
                                      account
                                      beg-time-string
                                      end-time-string
                                      drop-count)))
             (text (propertize balance-report
                               'font-lock-face
                               hledger-account-balance-expand-face)))
        (forward-line)
        (momentary-string-display text
                                  (point)
                                  ?\t
                                  "")))))


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

(defun hledger-refresh-buffer ()
  "Hack to refresh current report using `hledger-prev-report'."
  (interactive)
  (let ((hledger-last-run-time (1+ hledger-last-run-time)))
    (hledger-prev-report)))


(defun hledger-report-ending-today ()
  "Refresh report showing balances till today.
Usually, the balance shown are upto the the last
`hledger-reporting-date' starting the same date of the previous month."
  (interactive)
  (let ((hledger-reporting-day (string-to-number (format-time-string "%d"))))
    (hledger-refresh-buffer)))

(defun hledger-present-report ()
  "Reset time for the current report.
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
  "Return the top level account as a symbol from ACC-STRING."
  (car (split-string acc-string ":")))

(provide 'hledger-reports)
;;; hledger-reports.el ends here
