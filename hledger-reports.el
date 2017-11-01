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

(require 'calendar)
(require 'cl-lib)
(require 'pulse)

(require 'hledger-core)

(defconst hledger-jcompletions '("balancesheet"
                                 "daily"
                                 "incomestatement"
                                 "overall"
                                 "stats"
                                 "activity"
                                 "print"
                                 "accounts"
                                 "balance"
                                 "register")
  "Commands that can be passed to `hledger-jdo` function defined below.")

(defcustom hledger-extra-args " "
  "Extra arguments included while running Hledger for reports, e.g. -S."
  :group 'hledger
  :type 'string)

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

(defcustom hledger-year-of-birth 1992
  "Year in which you were born.
Required for calculating your age."
  :group 'hledger
  :type 'number)

(defcustom hledger-life-expectancy 80
  "Age upto which you expect to live."
  :group 'hledger
  :type 'number)

(defcustom hledger-show-only-unstarred-p t
  "Show only the un-tainted entries.
I taint entries with a star, to declare that they haven't been effective yet."
  :group 'hledger
  :type 'boolean)

(defcustom hledger-show-expanded-report t
  "Show expanded account balances in running report."
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

(defface hledger-report-header-face
  '((((class color) (background dark))
     :foreground "Cornsilk" :height 1.1)
    (((class color) (background light))
     :foreground "Black" :height 1.1)
    (t :inverse-video t))
  "Face for the header with date ranges in the the reports."
  :group 'hledger)

(defface hledger-overall-report-summary-text-face
  '((((class color) (background dark))
     :foreground "Cornsilk" :height 1.0)
    (((class color) (background light))
     :foreground "Black" :height 1.0)
    (t :inverse-video t))
  "Face for the summary text in overall report."
  :group 'hledger)

(defcustom hledger-account-balance-expand-face
  '(:foreground "Cornsilk" :background "DarkSlateGray")
  "Face for the expanded account names with their balances in current period."
  :group 'hledger
  :type 'face)

(defcustom hledger-ratios-net-worth-in-next-x-years
  5
  "Number of years for extrapolation of your net-worth."
  :group 'hledger
  :type 'number)

(defcustom hledger-extrapolate-savings-rate
  4.0
  "Rate of compound interest (in %) with which to extrapolate savings.
This is the annual rate of compound interest.  The bank may
choose to do the componding quarterly.  Configure
`hledger-extrapolate-savings-period' for that."
  :group 'hledger
  :type 'float)

(defcustom hledger-extrapolate-savings-period
  4
  "Number of months at which the interest is compounded."
  :group 'hledger
  :type 'float)

(defcustom hledger-width-spec
  '(100 . 40)
  "(# columns for the entry . # columns for description) for an entry."
  :group 'hledger
  :type 'string)

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

(defun hledger-beg-reporting-time ()
  "Return the beginning day for monthly reports."
  (let ((today (string-to-number (format-time-string "%d"))))
    (if (< hledger-reporting-day today)
        (hledger-nth-of-this-month hledger-reporting-day)
      (hledger-nth-of-prev-month hledger-reporting-day))))

(defun hledger-end-reporting-time ()
  "Return the end day for monthly reports."
  (let ((today (string-to-number (format-time-string "%d"))))
    (if (< hledger-reporting-day today)
        (hledger-nth-of-mth-month hledger-reporting-day 1)
      (hledger-nth-of-this-month hledger-reporting-day))))

(defun hledger-status (command-string)
  "Return the result of running COMMAND-STRING has an hledger command.

If the command failed, returns a cons with the error status and
the output to `standard-error' and `standard-output'."
  (with-temp-buffer
    (let ((status (call-process "hledger"
                                nil
                                t
                                nil
                                "-f"
                                (shell-quote-argument hledger-jfile)
                                command-string)))
      (if (= status 0)
          (buffer-string)
        ;; Error code and the error message
        (cons status (buffer-string))))))

(defun hledger-shell-command-to-string (command-string)
  "Return result of running hledger command COMMAND-STRING."
  (shell-command-to-string (concat "hledger -f " hledger-jfile " "
                                   command-string)))

(defun hledger-ask-and-save-buffer ()
  "Ask for saving modified buffer before any reporting commands."
  (if (and (eq major-mode 'hledger-mode)
           (buffer-modified-p)
           (yes-or-no-p (format "Save buffer %s? "
                                (buffer-name))))
      (save-buffer)
    (ignore)))

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

(defun hledger-run-command (command)
  "Run an hledger COMMAND."
  (interactive (list (completing-read "jdo> "
                                      hledger-jcompletions)))
  ;; Help other functions keep track of history.
  (setq hledger-last-run-command command)
  (hledger-ask-and-save-buffer)
  (let ((inhibit-read-only t))
    (pcase command
      (`"incomestatement" (hledger-monthly-incomestatement))
      (`"daily" (hledger-daily-report))
      (`"overall" (hledger-overall-report)
       (pop-to-buffer hledger-reporting-buffer-name)
       (delete-other-windows))
      (`"balancesheet" (hledger-jdo (concat "balancesheet --end "
                                            (hledger-end-date (current-time)))))
      ;; Allow account completion for
      (command (if (and (member command '("balance" "register"))
                        (called-interactively-p 'interactive))
                   (hledger-jdo-with-account-completion command)
                 (hledger-jdo command)))))
  (when (called-interactively-p 'interactive)
    (setq hledger-last-run-time 0))
  (pulse-momentary-highlight-region (point-min)
                                    (point-max)
                                    'next-error))

(defun hledger-get-accounts (&optional string)
  "Return list of account names with STRING infix present.
STRING can be multiple words separated by a space."
  (let* ((accounts-string (shell-command-to-string
                           (concat "hledger -f"
                                   hledger-jfile
                                   " accounts "
                                   (or string ""))))
         (accounts-list (split-string accounts-string)))
    accounts-list))

(defun hledger-get-balances (accounts)
  "Return balances for the sequence of ACCOUNTS."
  (with-current-buffer (hledger-jdo (mapconcat 'identity
                                               (cons "balance -N" accounts)
                                               " ")
                                    nil t)
    (font-lock-ensure)
    (let* ((report-str* (buffer-substring (point-min) (point-max))))
      (kill-buffer)
      report-str*)))

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
                          command
                          hledger-extra-args)))
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

(defun hledger-jdo-with-account-completion (command)
  "Run COMMAND with completions for account names."
  (let* ((crm-separator " ")
         (command-flags
          (completing-read-multiple (format "%s: " command)
                                    (mapcar (lambda (account)
                                              (concat account crm-separator))
                                            (or hledger-accounts-cache
                                                (setq hledger-accounts-cache
                                                      (hledger-get-accounts))))))
         (command-string (mapconcat 'identity
                                    (cons command command-flags)
                                    " ")))
    (hledger-run-command command-string)))

(defun hledger-jdo-redo-with (append-string)
  "Append APPEND-STRING to `hledger-last-run-command' and re-run."
  (hledger-run-command (format "%s%s"
                               hledger-last-run-command
                               append-string)))

(defun hledger-redo ()
  "Repeat the last command."
  (interactive)
  (hledger-jdo-redo-with ""))

(defvar hledger--ic 0
  "Variable to track increments in width for register command.")
(defun hledger-widen-results-for-register ()
  "Widen the results of the last command.
Works only for register command."
  (interactive)
  (if (not (string-prefix-p "register" hledger-last-run-command))
      (setq hledger--ic 0)
    (setq hledger--ic (+ hledger--ic 4))
    (hledger-run-command (format "%s --width %s,%s"
                                 hledger-last-run-command
                                 (+ (car hledger-width-spec)
                                    hledger--ic)
                                 (+ (cdr hledger-width-spec)
                                    (- hledger--ic 3))))))

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
          (end-time-string (hledger-end-date (current-time)))
          (hledger-underliner (make-string 20 ?═)))
      (hledger-jdo (format "balance %s --begin %s --end %s"
                           hledger-daily-report-accounts
                           beg-time-string
                           end-time-string))
      (goto-char (point-min))
      (insert (concat "Today you spent:\n"
                      hledger-underliner
                      "\n"))
      (goto-char (point-max))
      (insert (concat "\n\nSince "
                      (hledger-friendlier-time reporting-since)
                      "\n"
                      hledger-underliner
                      "\n"))
      (let ((beg (point)))
        (hledger-jdo (format "balance %s --begin %s --end %s --depth 2 --flat"
                             hledger-daily-report-accounts
                             (hledger-format-time reporting-since)
                             (hledger-end-date (current-time)))
                     t)
        (goto-char (point-max))
        (forward-line -3)
        (end-of-line)
        (ignore-errors
          (sort-numeric-fields 2 beg (point))
          (reverse-region beg (point))))
      (goto-char (point-min)))))

(defun hledger-monthly-incomestatement (&optional hide-header-p)
  "Incomestatement report but monthly.
You can have move back
and forth in time in the personal finance buffer.  I feel that the
complete incomestatement isn't much useful for me.
Optional argument HIDE-HEADER-P if non-nil, header line showing duration isn't shown."
  (interactive)
  (let* ((beg-time (hledger-beg-reporting-time))
         (end-time (hledger-end-reporting-time))
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
  (let* ((beg-time-string
          (hledger-format-time (hledger-nth-of-mth-month
                                hledger-reporting-day
                                (if (< (nth 3
                                            (decode-time (current-time)))
                                       hledger-reporting-day)
                                    (- hledger-running-report-months)
                                  (- 1 hledger-running-report-months)))))
         (end-time-string (hledger-format-time (hledger-end-reporting-time))))
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
      (when hledger-show-expanded-report
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
                     bury-bufferp)))))


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
  (or (lax-plist-get (hledger-compute-totals (list accounts-string) beg end)
                     accounts-string)
      0))

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

(defun hledger-compute-years-to-retirement* (spending-ratio)
  "Given SPENDING-RATIO, find number of years to retirement.
Configure `hledger-life-expectany' and `hledger-year-of-birth'.

SPENDING-RATIO = 1 - savings-ratio

The assumption is that you are going to keep spending the same
fraction of your income even after you retire.  This function
doesn't take into account the current savings that you have
accumulated so far."
  (let* ((this-year (nth 5 (decode-time (current-time))))
         (age (- this-year hledger-year-of-birth)))
    ;; It's amazing how simple this equation is. It's like with my spending
    ;; ratio, I am spending the rest of my days.
    (* spending-ratio (- hledger-life-expectancy age))))

(defun hledger-compute-years-to-retirement (savings monthly-expenses savings-ratio)
  "Compute years to retirement with SAVINGS, MONTHLY-EXPENSES and SAVINGS-RATIO."
  (- (hledger-compute-years-to-retirement* (- 1 savings-ratio))
     ;; Buying some years of my life with current savings
     (/ savings monthly-expenses 12.0)))

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
         (reporting-date-now (hledger-end-date (hledger-end-reporting-time)))

         (totals-plist-1 (hledger-compute-totals
                          (list hledger-ratios-assets-accounts
                                hledger-ratios-income-accounts
                                hledger-ratios-essential-expense-accounts)
                          reporting-date-an-year-ago
                          reporting-date-now))

         ;; For average balances
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
         (monthly-savings (- monthly-income monthly-total-expenses))

         (savings-ratio (/ monthly-savings monthly-income))
         (current-net-worth (- total-assets liabilities))
         (years-to-retirement (hledger-compute-years-to-retirement current-net-worth
                                                                   monthly-total-expenses
                                                                   savings-ratio)))
    (list 'avg-income (* monthly-income 1.0)                        ;; Monthly income
          'liquid-assets liquid-assets                              ;; Liquid\
          'total-assets total-assets                                ;; Total /  Assets
          'liabilities liabilities
          'avg-expenses (* monthly-total-expenses 1.0)              ;; Average expenses
          'avg-monthly-savings monthly-savings                      ;; Average monthly savings
          'total-assets total-assets                                ;; Total assets as of now
          'current-net-worth current-net-worth                      ;; Current net worth
          'efr (/ liquid-assets (* monthly-essential-expenses 1.0)) ;; Emergency-fund-ratio
          'tfr (/ liquid-assets (* monthly-total-expenses 1.0))     ;; Total-fund ratio | Similar to efr.
          'br (/ total-assets monthly-total-expenses)               ;; Bankruptcy ratio
          'cr  (/ liquid-assets (* liabilities 1.0))                ;; Current ratio
          'sr  savings-ratio                                        ;; Savings ratio
          'ytr years-to-retirement                                  ;; Years I will have to keep working for
          'dr (/ liabilities (* total-assets 1.0)))))               ;; Debt ratio

(defun hledger-break-lines (s &optional separator width)
  "Add newline characters to string S.
Optional parameter WIDTH decides the maximum width of a line."
  (let* ((width* (or width 80))
         (init (seq-take s width*))
         (end-index (string-match " [^ ]*$" init)))
    (if (and end-index ;; It's not a single word.
             (< width* (length s)))
        (concat (seq-take s end-index)
                (or separator "\n  ")
                (hledger-break-lines (seq-drop s end-index)))
      s)))

(defun hledger-compound-money (init periods periodic-rate)
  "Compound INIT amount for PERIODS units at PERIODIC-RATE.

PERIODIC-RATE is in % and hence must be divided by 100."
  (* (or init 0)
     (expt (+ 1 (/ periodic-rate 100.0)) periods)))

(defun hledger-extrapolate-monthly-savings (monthly-savings
                                            n
                                            &optional initial-sum)
  "Total savings with interest for MONTHLY-SAVINGS in N months.

I live in India where bank do compounding quarterly with an
interest rate of 4.0% per year.  Configure
`hledger-extrapolate-savings-rate' and
`hledger-extrapolate-savings-period' accordingly.

Formula: Future value of an annuity = P ([(1 + r)^n - 1]/r).
This assumes that the first payment comes at the end of first
period.

Optional argument INITIAL-SUM is the amount you have now.  You will
earn interest on this amount as well."
  (let* ((quarters (/ n 4.0))
         (quarterly-rate% (/ hledger-extrapolate-savings-rate 4.0))
         (quarterly-rate (/ quarterly-rate% 100.0))
         (quarterly-savings (* monthly-savings 4.0)))
    ;; Initial sum
    (+ (hledger-compound-money initial-sum
                               quarters
                               quarterly-rate%)
       ;; Future value of Annuity
       (* quarterly-savings
          (/ (- (expt (+ 1.0 quarterly-rate) quarters) 1.0)
             quarterly-rate)))))

(defun hledger-summarize-ratios (ratios)
  "Return a string summary of RATIOS."
  (let* ((tfr (plist-get ratios 'tfr))
         (br (plist-get ratios 'br))
         (cr (plist-get ratios 'cr))
         (dr (plist-get ratios 'dr))
         (sr (plist-get ratios 'sr))
         (cnw (plist-get ratios 'current-net-worth))
         (avg-monthly-savings (plist-get ratios 'avg-monthly-savings))
         (extrapolated-savings
          (* 12
             hledger-ratios-net-worth-in-next-x-years
             avg-monthly-savings))
         (extrapolated-net-worth (+ cnw
                                    extrapolated-savings))
         (extrapolated-net-worth-with-compounding
          (hledger-extrapolate-monthly-savings avg-monthly-savings
                                               (* 12 hledger-ratios-net-worth-in-next-x-years)
                                               cnw))
         (summary-string
          (format
           (concat
            (make-string 80 ?═)
            "\b • Your liquid assets would last %s and total assets %s with this lifestyle. \b\
 • Your liquid assets are %.2f times your liabilities/debt. \b\
 • %.2f%% of your total assets are borrowed. \b\
 • For the past one year, you have been saving %.2f%% of your average income. \b\
 • Your assets would roughly increase by %s %s in the next %s years making your net worth %s %s.\
 If compounded every %s months at %s%% per annum, your net worth would become %s %s. \b"
            (make-string 80 ?═) "\n")
           ;; @TODO: Show a message asking the user to customize 'hledger
           ;; group
           (or (ignore-errors (hledger-humanize-float-months tfr))
               "nan")
           (or (ignore-errors (hledger-humanize-float-months br))
               "nan")
           cr
           (* dr 100.0)
           (* sr 100.0)
           hledger-currency-string
           (or (ignore-errors (hledger-group-digits (truncate extrapolated-savings)))
               "nan")
           hledger-ratios-net-worth-in-next-x-years
           hledger-currency-string
           (or (ignore-errors (hledger-group-digits (truncate extrapolated-net-worth)))
               "nan")
           hledger-extrapolate-savings-period
           hledger-extrapolate-savings-rate
           hledger-currency-string
           (or (ignore-errors
                 (hledger-group-digits
                  (truncate
                   extrapolated-net-worth-with-compounding)))
               "nan"))))
    (mapconcat 'identity
               (mapcar 'hledger-break-lines (split-string summary-string "\b"))
               "\n")))

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

             (this-year (nth 5 (decode-time (current-time))))
             (age (- this-year hledger-year-of-birth))
             (ytr (plist-get ratios 'ytr))
             (retiring-at (+ age ytr))

             (avg-income (plist-get ratios 'avg-income))
             (avg-expenses (plist-get ratios 'avg-expenses))
             (liquid-assets (plist-get ratios 'liquid-assets))
             (total-assets (plist-get ratios 'total-assets))
             (liabilities (plist-get ratios 'liabilities))
             (current-net-worth (plist-get ratios 'current-net-worth))
             (summary (hledger-summarize-ratios ratios)))
        (goto-char (point-min))
        (forward-line 2)
        (insert (format "
╔══════════════════════════════════════╦══════════════════════════════════════════╗

   Emergency Fund Ratio: %-18.2fSavings Ratio: %.2f
   Current Ratio: %-25.2fAverage Income: %s %.0f/month
   Debt Ratio: %-28.2fAverage Expenses: %s %.0f/month
   ──────────────────────────────────────────────────────────────────
   Liquid Assets: %s %-23.2fTotal Assets: %s %.2f
   Liabilities: %s %-25.2fNet Worth: %s %.2f
   ──────────────────────────────────────────────────────────────────
   Years to retirement: %-19.0fRetiring at: %.0f
   Age:%-36.0fLife Expectancy: %.0f

╚══════════════════════════════════════╩══════════════════════════════════════════╝

%s
"
                        efr sr
                        cr  hledger-currency-string avg-income
                        dr  hledger-currency-string avg-expenses
                        hledger-currency-string liquid-assets
                        hledger-currency-string total-assets
                        hledger-currency-string liabilities
                        hledger-currency-string current-net-worth
                        ytr
                        retiring-at
                        age
                        hledger-life-expectancy
                        (propertize summary
                                    'font-lock-face
                                    'hledger-overall-report-summary-text-face))))
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
                                     ?═)))
    (propertize (format "%s %s\n%s=\n\n"
                        header-title
                        header-dates
                        header-filler)
                'font-lock-face 'hledger-report-header-face)))

(defun hledger-expand-account ()
  "Expands account for the month according to `hledger-last-run-time'."
  (interactive)
  (if (equal hledger-last-run-command "daily")
      (hledger-run-fn-for-day hledger-last-run-time
                              'hledger-expand-account-for-this-month)
    (hledger-run-fn-for-month hledger-last-run-time
                              'hledger-expand-account-for-this-month)))

(defun hledger-expand-account-for-this-month ()
  "Expand the balance for account in the current line."
  (save-excursion
    (forward-line 0)
    (when (search-forward-regexp hledger-account-regex (line-end-position) t)
      (let* ((account (substring-no-properties (match-string 0)))
             (drop-count (length (split-string account ":")))
             (beg-time (hledger-beg-reporting-time))
             (end-time (hledger-end-reporting-time))
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
    (`"balancesheet" (hledger-run-command-for-day
                                           hledger-last-run-time
                                           hledger-last-run-command))
    (_ (hledger-run-command-for-month hledger-last-run-time
                                      hledger-last-run-command)))
  (pulse-momentary-highlight-region (point-min)
                                    (point-max)
                                    'next-error))

(defun hledger-next-report ()
  "Takes your report forward in time.
See `hledger-prev-report'."
  (interactive)
  (setq hledger-last-run-time (1+ hledger-last-run-time))
  (pcase hledger-last-run-command
    (`"daily" (hledger-run-command-for-day hledger-last-run-time
                                           hledger-last-run-command))
    (`"balancesheet" (hledger-run-command-for-day hledger-last-run-time
                                           hledger-last-run-command))
    (_ (hledger-run-command-for-month hledger-last-run-time
                                      hledger-last-run-command)))
  (pulse-momentary-highlight-region (point-min)
                                    (point-max)
                                    'next-error))

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
